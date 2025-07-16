Feature: Customizations
  Scenario: Line length limit
    Given emacs loads
    And test buffer is "customization"
    And buffer contains "abc " repeated "40" times
    And filling paragraph is called

    Then buffer contains "2" same lines long "79" chars
