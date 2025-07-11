Feature: Config loading
  Scenario: Loads without failure
    Given dot emacs exists
    When it loads from scratch
    Then it should install Elpaca
    And it should install packages
