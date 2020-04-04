
8.12.1 [[GetOwnProperty]] (P)

When the [[GetOwnProperty]] internal method of O is called
with property name P, the following steps are taken:

    1. If O doesn't have an own property with name P,
       return undefined.
    2. Let D be a newly created Property Descriptor with
       no fields.
    3. Let X be O's own property named P.
    4. If X is a data property, then
        a. Set D.[[Value]] to the value of X's [[Value]]
           attribute.
        b. Set D.[[Writable]] to the value of X's
           [[Writable]] attribute
    5. Else X is an accessor property, so
        a. Set D.[[Get]] to the value of X's [[Get]]
           attribute.
        b. Set D.[[Set]] to the value of X's [[Set]]
           attribute.
    6. Set D.[[Enumerable]] to the value of X's
       [[Enumerable]] attribute.
    7. Set D.[[Configurable]] to the value of X's
       [[Configurable]] attribute.
    8. Return D.

However, if O is a String object it has a more elaborate
[[GetOwnProperty]] internal method defined in 15.5.5.2.
