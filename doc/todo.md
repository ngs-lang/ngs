Next items for immediate implementation
=======================================

Roughly in priorities order:

* Cleanup unused files
* Rename project to "ngs" (including github move)
* Allow running scripts via the API
* `package.json`
* Language implementation (`syntax.js`, `compile.js`, ...)
    * Add debug info so good error messages could be generated
        * Info to pass
            * Line
            * Col
            * Function name
        * When building the tree
        * When generating code
    * Variables
        * Exception when using unbound variables
        * Lexical scopes
    * Handle exceptions
    * Implement internal API so the langauge and REST will use it
