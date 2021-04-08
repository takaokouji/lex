# frozen_string_literal: true

require_relative "../lexeme"
require_relative "../logger"
require_relative "../state"

module Lex
  class Lexer
    # Rules DSL used internally by {Lexer}
    #
    # @api private
    class RuleDSL
      attr_reader :lex_tokens,
                  :state_info,
                  :state_re,
                  :state_names,
                  :state_ignore,
                  :state_error,
                  :state_lexemes,
                  :logger,
                  :options,
                  :eof_actions

      # @api private
      def initialize
        @state_info     = { initial: :inclusive }
        @state_ignore   = { initial: '' }  # Ignored characters for each state
        @state_error    = {} # Error conditions for each state
        @state_re       = Hash.new { |hash, name| hash[name] = {}} # Regexes for each state
        @state_names    = {} # Symbol names for each state
        @state_lexemes  = Hash.new { |hash, name| hash[name] = State.new(name) }
        @lex_tokens     = []  # List of valid tokens
        @logger         = Lex::Logger.new
        @options        = Set.new
        @current_states = nil
        @eof_actions    = []
      end

      # Specify lexing option
      #
      # @param [Symbol] name
      #   the option name
      #
      # @api public
      def option(name)
        @options << name
      end

      # Add inclusive states to lexer
      #
      # @param [Symbol, Array<Symbol>] names
      #   an array of inclusive state names
      #
      # @api public
      def inclusive_states(names)
        @state_info.merge!(Array(names).to_h { |n| [n, :inclusive] })
      end

      alias_method :s, :inclusive_states

      # Add exclusive states to lexer
      #
      # @param [Symbol, Array<Symbol>] names
      #   an array of exclusive state names
      #
      # @api public
      def exclusive_states(names)
        @state_info.merge!(Array(names).to_h { |n| [n, :exclusive] })
      end

      alias_method :x, :exclusive_states

      # Add tokens to lexer
      #
      # @api public
      def tokens(*value)
        @lex_tokens = value
      end

      # Add states to lexer
      #
      # @api public
      def states(value)
        @state_info.merge!(value)
      end

      # Add states to lexer
      #
      # @param [Symbol, Array<Symbol>] names
      #   an array of state names for specify lexing rule
      #
      # @api public
      def rule_for(names, &block)
        complain("Already specified '#{@current_states.join(', ')}' states") if @current_states

        @current_states = Array(names)
        begin
          block.call
        ensure
          @current_states = nil
        end
      end

      # Specify lexing rule
      #
      # @param [Symbol] name
      #   the rule name
      #
      # @param [Regex] pattern
      #   the regex pattern
      #
      # @api public
      def rule(*args, &action)
        return rule_name_pattern_action(*args, &action) if !action || action.arity == 2

        complain("Specify an action") if !action

        state_names_pattern = []
        states = @current_states || []
        args.each do |arg|
          case arg
          when Symbol
            complain("Already specified '#{@current_states.join(', ')}' states") if @current_states

            name = arg
            if name == :*
              states = @state_info.keys
            else
              states << name
            end
          when Regexp, String
            if arg.is_a?(Regexp)
              re = arg
            else
              re = Regexp.new(Regexp.quote(arg))
            end
            re = /#{re.source}/i if @options.include?(:caseless)

            if states.empty?
              states = [:initial]
            else
              states.uniq!
              if states.include?(:INITIAL)
                states.delete(:INITIAL)
                states << :initial
              end
            end
            state_names_pattern << [states, re]

            states = @current_states || []
          end
        end

        complain("Specify a pattern") if state_names_pattern.empty?

        case action.arity
        when 0
          action_0 = action
          action_2 = ->(lexer, token) {
            lexer.instance_exec(&action_0)
          }
        when 1
          action_1 = action
          action_2 = ->(lexer, token) {
            lexer.instance_exec(token, &action_1)
          }
        else
          complain("Specified action takes an invalid number of arguments")
        end
        action = ->(lexer, token) {
          result = action_2.call(lexer, token)
          if token.name
            token
          else
            case result
            when Symbol
              token.name = result
              token
            when String
              token.name = result
              token.value = result
              token
            else
              nil
            end
          end
        }

        state_names_pattern.each do |state_names, pattern|
          state_names.each do |state_name|
            state = @state_lexemes[state_name]
            state << Lexeme.new(nil, pattern, &action)
          end
        end

        update_inclusive_states
      end

      # Specify action when the EOF is reached
      #
      # @api public
      def eof(&action)
        @eof_actions << action if action
      end

      # Define ignore condition for a state
      #
      # @param [Symbol] states
      #   the optional state names
      #
      # @param [String] value
      #   the characters to ignore
      #
      # @api public
      def ignore(states, value = (not_set = true))
        if not_set
          value = states
          state_names = [:initial]
        else
          state_names = states.to_s.split('_').map(&:to_sym)
        end
        if !value.is_a?(String)
          logger.error("Ignore rule '#{value}' has to be defined with a string")
        end
        state_names.each do |state_name|
          @state_ignore[state_name] = value
        end
        @state_info.each do |state_name, state_type|
          if state_name != :initial && state_type == :inclusive
            if !@state_ignore.key?(state_name)
              @state_ignore[state_name] = @state_ignore[:initial]
            end
          end
        end
      end

      # Define error condition for a state
      #
      # @api public
      def error(states = :initial, &action)
        state_names = states.to_s.split('_').map(&:to_sym)
        state_names.each do |state_name|
          @state_error[state_name] = action
        end
        @state_info.each do |state_name, state_type|
          if state_name != :initial && state_type == :inclusive
            if !@state_error.key?(state_name)
              @state_error[state_name] = @state_error[:initial]
            end
          end
        end
      end

      private

      # @api private
      def rule_name_pattern_action(name, pattern, &action)
        state_names, token_name = *extract_state_token(name)
        if token_name =~ /^[[:upper:]]*$/ && !@lex_tokens.include?(token_name)
          complain("Rule '#{name}' defined for" \
            " an unspecified token #{token_name}")
        end
        state_names.each do |state_name|
          state = @state_lexemes[state_name]
          state << Lexeme.new(token_name, pattern, &action)
        end
        update_inclusive_states
        state_names.each do |state_name|
          if @state_re[state_name].key?(token_name)
            complain("Rule '#{name}' redefined.")
          end
          @state_re[state_name][token_name] = pattern
        end
      end

      # For inclusive states copy over initial state rules
      #
      # @api private
      def update_inclusive_states
        @state_info.each do |state_name, state_type|
          if state_name != :initial && state_type == :inclusive
            initial_state = @state_lexemes[:initial]
            @state_lexemes[state_name].update(initial_state.lexemes)
          end
        end
      end

      # Extract tuple of state names and token name
      #
      # @param [Symbol] name
      #   the rule name
      #
      # @return [Array[Symbol], Symbol]
      #   returns tuples [states, token]
      #
      # @api private
      def extract_state_token(name)
        parts = name.to_s.split('_')
        state_i = 0
        parts.each_with_index do |part, i|
          if !@state_info.keys.include?(part.to_sym)
            state_i = i
            break
          end
        end
        states = if state_i > 0
                   parts[0...state_i].map(&:to_sym)
                 else
                   [:initial]
                 end
        token_name = parts[state_i..-1].join('_').to_sym
        [states, token_name]
      end

      # @api private
      def complain(*args)
        raise LexerError, *args
      end
    end # RuleDSL
  end # Lexer
end # Lex
