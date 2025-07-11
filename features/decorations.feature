Feature: Frame and window decorations
  Scenario: Minimal inner decorations
    When emacs loads
    Then toolbar is hidden
    And scrollbar is hidden
    And file dialog is hidden
    And fringe is "8" pixels wide

  Scenario: Minimal outer decorations
    When emacs loads
    Then frame is undecorated

  Scenario: 24-hours time
    When emacs loads
    Then window mode line displays time
    And window mode line time is in "24-hours" format
