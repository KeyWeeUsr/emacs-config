Feature: Keyboard shortcuts
  Scenario: Opening a multi-term instance for current directory in any buffer
    Given emacs loads
    And test buffer is "global-side-terminal"
    And selected buffer is "global-side-terminal"
    Then selected buffer should be "global-side-terminal"

    When I press "C-c |" in buffer "global-side-terminal"
    Then selected buffer should be "*terminal<1>*"

    When I press "C-x 4 0" in buffer "*terminal<1>*" without queries
    Then no multi-term buffer should remain open
    And selected buffer should be "global-side-terminal"
