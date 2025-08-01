Feature: Keyboard shortcuts
  @not-runnable-on-27.1 @not-runnable-on-27.2
  Scenario: Opening a multi-term instance for current directory in any buffer
    Given emacs loads
    And test buffer is "global-side-terminal"
    And selected buffer is "global-side-terminal"
    Then selected buffer should be "global-side-terminal"

    When I press "C-c |"
    Then selected buffer should be "*terminal<1>*"

    When I press "C-x 4 0" without queries
    Then no multi-term buffer should remain open
    And selected buffer should be "global-side-terminal"

  Scenario: Multi-term sending SIGINT (^C)
    Given emacs loads
    And multi-term terminal launches
    And test buffer is multi-term
    And emacs reads output

    When I press "echo"
    And emacs reads output
    And I press "SPC"
    And emacs reads output
    And I press "$?"
    And emacs reads output
    And I press "RET"
    And emacs reads output
    Then buffer should contain "# echo $?\n0\n# "

    When I press "C-c C-c"
    And emacs reads output
    Then binding "C-c C-c" should exist in map "term-raw-map"
    And binding "C-c C-c" should exist in map "current-local-map"

    When I press "echo"
    And emacs reads output
    And I press "SPC"
    And emacs reads output
    And I press "$?"
    And emacs reads output
    And I press "RET"
    And emacs reads output
    Then buffer should contain "# echo $?\n0\n# ^C\n# echo $?\n130\n# "

    When I press "C-d"
    And emacs reads output
    Then no multi-term buffer should remain open

  Scenario: Multi-term switching between buffer and line mode
    Given emacs loads
    And multi-term terminal launches
    And test buffer is multi-term
    And emacs reads output

    Then point should be at "line-end"
    And multi-term should be in "char" mode

    When I press "C-c C-k"
    And emacs reads output
    Then point should be at "line-end"
    And multi-term should be in "char" mode

    When I press "C-c C-j"
    And emacs reads output
    Then point should be at "line-beginning"
    And multi-term should be in "line" mode

    When I press "C-c C-k"
    And emacs reads output
    Then point should be at "line-end"
    And multi-term should be in "char" mode

    When I press "C-d"
    And emacs reads output
    Then no multi-term buffer should remain open

  Scenario: Helpful finds a callable (C-h f)
    Given emacs loads
    And test buffer is "callable"

    When I press "C-h f"
    And I press "abc" in minibuffer
    And I press "RET" in minibuffer
    Then helpful should open "callable"

  Scenario: Helpful finds a variable (C-h v)
    Given emacs loads
    And test buffer is "callable"

    When I press "C-h v"
    And I press "abc" in minibuffer
    And I press "RET" in minibuffer
    Then helpful should open "variable"

  Scenario: Helpful finds a keybinding (C-h k)
    Given emacs loads
    And test buffer is "callable"

    When I press "C-h k"
    And I press "abc" in minibuffer
    And I press "RET" in minibuffer
    Then helpful should open "key"

  Scenario: Helpful finds a command (C-h x)
    Given emacs loads
    And test buffer is "callable"

    When I press "C-h x"
    And I press "abc" in minibuffer
    And I press "RET" in minibuffer
    Then helpful should open "command"

  Scenario: Window enlarges horizontally
    Given emacs loads
    And test buffer is "enlarges-horizontally"
    And selected buffer is "enlarges-horizontally"

    When I press "C-c <right>"
    And emacs reads output
    Then window should change size with "enlarge-window-horizontally"

  Scenario: Window shrinks horizontally
    Given emacs loads
    And test buffer is "shrink-horizontally"
    And selected buffer is "shrink-horizontally"

    When I press "C-c <left>"
    And emacs reads output
    Then window should change size with "shrink-window-horizontally"

  Scenario: Window enlarges vertically
    Given emacs loads
    And test buffer is "enlarges-vertically"
    And selected buffer is "enlarges-vertically"

    When I press "C-c <down>"
    And emacs reads output
    Then window should change size with "enlarge-window"

  Scenario: Window shrinks vertically
    Given emacs loads
    And test buffer is "shrink-vertically"
    And selected buffer is "shrink-vertically"

    When I press "C-c <up>"
    And emacs reads output
    Then window should change size with "shrink-window"
