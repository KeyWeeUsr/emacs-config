Feature: Editing aid
  # note: (format-mode-line) unfortunately requires C-based !noninteractive
  #       github.com/emacs-mirror/emacs/blob/db46d5e/src/xdisp.c#L28617
  @interactive-only
  Scenario: Rows and columns are on the mode line
    Given emacs loads
    And temp buffer "rows-and-cols" contains "hello\nthere\n"

    Then minor mode "line-number-mode" should be activated
    And minor mode "column-number-mode" should be activated
    And lighter in buffer "rows-and-cols" at "<point>" should show "<rowcol>":
      |     point | rowcol |
      |         1 | (1,0)  |
      |         3 | (1,2)  |
      |         6 | (1,5)  |
      |         7 | (2,0)  |
      |         9 | (2,2)  |
      |        12 | (2,5)  |
      |        13 | (3,0)  |
      | point-max | (3,0)  |

  Scenario: Overwriting selections
    When emacs loads

    Then minor mode "delete-selection-mode" should be activated
    And minor mode "transient-mark-mode" should be activated

    When temp buffer "overwriting" contains "hello\nthere"
    And point in buffer "overwriting" is at "point-max"
    # M-S-b
    And I select previous word in buffer "overwriting"
    Then mark in temp buffer "overwriting" should be active
    And region in temp buffer "overwriting" should be active
    And active region in temp buffer "overwriting" should select "((7 . 12))"

    When I type "world" in buffer "overwriting"
    Then temp buffer "overwriting" should contain "hello\nworld"

  Scenario: Too long lines
    Given emacs loads
    # nitpick: cheap check, expand into an exact feature to allow lib swapping
    Then minor mode "global-so-long-mode" should be activated

  Scenario: Shortcuts for Org mode blocks
    Given emacs loads
    And temp buffer "org-shortcuts" contains ""
    And mode "org-mode" is activated
    And advice for "read-string" returns "ASK"

    Then inserting "<text>" and pressing "TAB" should convert to:
      | text | result                                                                   |
      | <a   | #+begin_export ascii\nP\n#+end_export                                    |
      | <c   | #+begin_center\nP\n#+end_center                                          |
      | <e   | #+begin_example\nP\n#+end_example                                        |
      | <h   | #+begin_export html\nP\n#+end_export                                     |
      | <i   | #+index: P                                                               |
      | <l   | #+begin_export latex\nP\n#+end_export                                    |
      | <q   | #+begin_quote\nP\n#+end_quote                                            |
      | <v   | #+begin_verse\nP\n#+end_verse                                            |
      | <s   | #+name: ASK\n#+begin_src ASK :results output :exports both\nP\n#+end_src |

  Scenario: Parenthesis pair is highlighted with point on
    Given emacs loads

    When temp buffer "<name>" contains "<contents>":
      | name                     | contents |
      | left-missing             | (        |
      | missing-right            | )s       |
      | left-right               | ()s      |
      | left-left-right-missing  | (()s     |
      | missing-left-right-right | ())s     |

    # highlighting triggers at opening paren and one place after closing paren
    Then temp buffer "<name>" in show-paren-mode should be highlighted by "<highlight>":
      | name                     | highlight                        |
      | left-missing             | 1:1:mismatch                     |
      | missing-right            | 2:1:mismatch                     |
      | left-right               | 1:1:match,3:2:match              |
      | left-left-right-missing  | 1:1:mismatch,2:2:match,4:3:match |
      | missing-left-right-right | 1:1:match,3:2:match,4:3:mismatch |

  Scenario: Opening a pull request through remote reference
    Given emacs loads
    And multi-term terminal launches
    And multi-term buffer contains "Create a pull request\nremote: http://localhost\nremote:"
    And point in buffer "multi-term" is at "point-max"

    When I press "C-c C-o" in buffer "multi-term"
    Then browser should open "http://localhost" url

    When I press "C-d" in buffer "multi-term"
    Then no multi-term buffer should remain open
