Feature: Spaces
  Scenario: Pressing tab by default inserts fixed amount of spaces
    Given emacs loads
    And test buffer is "default-tab"

    # note: unnecessary to add loops and what not for what it should really do
    When I press "TAB" in test buffer
    Then buffer should contain "    "

    When I press "TAB" in test buffer
    Then buffer should contain "        "

    When I press "TAB" in test buffer
    Then buffer should contain "            "

    When I press "TAB" in test buffer
    Then buffer should contain "                "

    When I press "TAB" in test buffer
    Then buffer should contain "                    "

    When I press "TAB" in test buffer
    Then buffer should contain "                        "

    When I press "TAB" in test buffer
    Then buffer should contain "                            "

    When I press "TAB" in test buffer
    Then buffer should contain "                                "

    When I press "TAB" in test buffer
    Then buffer should contain "                                    "

    When I press "TAB" in test buffer
    Then buffer should contain "                                        "

    When I press "TAB" in test buffer
    Then buffer should contain "                                            "

    When I press "<backtab>" in test buffer
    Then buffer should contain "                                        "

    When I press "<backtab>" in test buffer
    Then buffer should contain "                                    "

    When I press "<backtab>" in test buffer
    Then buffer should contain "                                "

    When I press "<backtab>" in test buffer
    Then buffer should contain "                            "

    When I press "<backtab>" in test buffer
    Then buffer should contain "                        "

    When I press "<backtab>" in test buffer
    Then buffer should contain "                    "

    When I press "<backtab>" in test buffer
    Then buffer should contain "                "

    When I press "<backtab>" in test buffer
    Then buffer should contain "            "

    When I press "<backtab>" in test buffer
    Then buffer should contain "        "

    When I press "<backtab>" in test buffer
    Then buffer should contain "    "

    When I press "<backtab>" in test buffer
    Then buffer should contain ""

  Scenario: Indenting with spaces
    Given emacs loads
    And test buffer is "custom-tab"
    And local variable "tab-width" is "2"
    And local variable "tab-stop-list" is "(2 4 6 8 10 12 14 16 18 20 22)"

    When I press "TAB" in test buffer
    Then buffer should contain "  "

    When I press "TAB" in test buffer
    Then buffer should contain "    "

    When I press "TAB" in test buffer
    Then buffer should contain "      "

    When I press "TAB" in test buffer
    Then buffer should contain "        "

    When I press "TAB" in test buffer
    Then buffer should contain "          "

    When I press "TAB" in test buffer
    Then buffer should contain "            "

    When I press "TAB" in test buffer
    Then buffer should contain "              "

    When I press "TAB" in test buffer
    Then buffer should contain "                "

    When I press "TAB" in test buffer
    Then buffer should contain "                  "

    When I press "TAB" in test buffer
    Then buffer should contain "                    "

    When I press "TAB" in test buffer
    Then buffer should contain "                      "

    When I press "<backtab>" in test buffer
    Then buffer should contain "                    "

    When I press "<backtab>" in test buffer
    Then buffer should contain "                  "

    When I press "<backtab>" in test buffer
    Then buffer should contain "                "

    When I press "<backtab>" in test buffer
    Then buffer should contain "              "

    When I press "<backtab>" in test buffer
    Then buffer should contain "            "

    When I press "<backtab>" in test buffer
    Then buffer should contain "          "

    When I press "<backtab>" in test buffer
    Then buffer should contain "        "

    When I press "<backtab>" in test buffer
    Then buffer should contain "      "

    When I press "<backtab>" in test buffer
    Then buffer should contain "    "

    When I press "<backtab>" in test buffer
    Then buffer should contain "  "

    When I press "<backtab>" in test buffer
    Then buffer should contain ""
