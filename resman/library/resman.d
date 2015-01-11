/**
 * Coedit resource API
 */
module resman;

/**
 * Activate the resman mechanism. If the resources related to a
 * module have been generated using the Coedit *Resman* widget
 * then mixing this template will:
 * - import this module, thus the method to access the to resources
 * - import, at compile time, the resource data, located in the .res file
 * Examples:
 * ---
 * mixin activateResman;
 * ---
 */
mixin template activateResman()
{
    mixin("private import resman;");
    enum f = (__FILE__.stripExtension.stripPath) ~ ".res";
    mixin(import(f));
}

public enum ResFormat {
    bytes,  //
    utf8,   //
    base16, //
    base64  //
}

/**
 *
 * Params:
 * identifiers = the array which holds the resource identifiers, 
 * always named *residententifiers*.
 * identifier = the identifier to find.
 *
 * Return:
 * a positive value if the resource is found otherwise -1. 
 */
public ptrdiff_t resourceIndex(string[] identifiers, string identifier)
{
    return -1;
}


