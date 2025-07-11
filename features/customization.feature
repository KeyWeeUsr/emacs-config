Feature: Customizations
  Scenario: Line length limit
    Given emacs loads
    And test buffer contains "abc " repeated "40" times
    And filling paragraph is called

    Then test buffer contains "2" same lines long "79" chars
