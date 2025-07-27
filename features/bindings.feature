Feature: Keyboard shortcuts
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
