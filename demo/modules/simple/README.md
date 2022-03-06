# Modules demo

These demos demonstrate the use of modules. There are 3 demos which
basically do the same thing, just using different techniques.

Functionally the module offers 2 things:

* a way to clear HTML below a certain tag (identified by it's ID): the **ClearPage** function.
  the **DefaultClearID** variable is used to determine the HTML tag if no tag is specified in the **ClearPage** function.

* a way to set the HTML page's title which is visible in the tab caption or browser window
  title bar. This is done with the **SetPageTitle** function.

## htmlutils

In the *htmlutils* demo, the 2 functions and a variable are exported:
* ClearPage
* SetPageTitle
* DefaultClearID

## Classes

In the *classes* demo, the 2 functions and a variable are made part of a class
 **THTMLUtils** , and only a function to create an instance of this class is exported.

Exported:
* CreateUtils

## ClassesUsingVar

In the *classesusingvar* demo, the function to create an object is removed and
replaced by a variable that contains an instance of the **THTMLUtils** class.
This variable is then exported.

Exported:
* Utils
