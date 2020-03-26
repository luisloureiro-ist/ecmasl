
function NewPropertyDescriptor() {
  return {}
};
/**
 * 8.10 The Property Descriptor and Property Identifier Specification Types
 *
 * The Property Descriptor type is used to explain the manipulation and reification of named property attributes.
 * Values of the Property Descriptor type are records composed of named fields where each field’s name is an attribute name and
 * its value is a corresponding attribute value as specified in 8.6.1. In addition, any field may be present or absent.
 *
 * Property Descriptor values may be further classified as data property descriptors and accessor property descriptors based
 * upon the existence or use of certain fields. A data property descriptor is one that includes any fields named
 * either [[Value]] or [[Writable]]. An accessor property descriptor is one that includes any fields named
 * either [[Get]] or [[Set]]. Any property descriptor may have fields named [[Enumerable]] and [[Configurable]].
 *
 * A Property Descriptor value may not be both a data property descriptor and an accessor property descriptor;
 * however, it may be neither. A generic property descriptor is a Property Descriptor value that is neither a data property descriptor
 * nor an accessor property descriptor. A fully populated property descriptor is one that is either an accessor property descriptor
 * or a data property descriptor and that has all of the fields that correspond to the property attributes defined in either 8.6.1 Table 5 or Table 6.
 *
 * For notational convenience within this specification, an object literal-like syntax can be used to define a property descriptor value.
 * For example, Property Descriptor {[[Value]]: 42, [[Writable]]: false, [[Configurable]]: true} defines a data property descriptor.
 *
 * Field name order is not significant. Any fields that are not explicitly listed are considered to be absent.
 * In specification text and algorithms, dot notation may be used to refer to a specific field of a Property Descriptor.
 * For example, if D is a property descriptor then D.[[Value]] is shorthand for “the field of D named [[Value]]”.
 *
 * The Property Identifier type is used to associate a property name with a Property Descriptor.
 * Values of the Property Identifier type are pairs of the form (name, descriptor), where name is a String and descriptor is a Property Descriptor value.
 * The following abstract operations are used in this specification to operate upon Property Descriptor values:
 */

/**
 * 8.10.1 IsAccessorDescriptor ( Desc )
 *
 * When the abstract operation IsAccessorDescriptor is called with property descriptor Desc, the following steps are taken:
 */
function IsAccessorPropertyDescriptor (Desc) {
  /** 1. If Desc is undefined, then return false. */
  if (Desc = undefined) {
    return false
  };

  /** 2. If both Desc.[[Get]] and Desc.[[Set]] are absent, then return false. */
  if (!(("Get" in Desc) && ("Set" in Desc))) {
    return false
  };

  /** 3. Return true. */
  return true
};

/**
 * 8.10.2 IsDataDescriptor ( Desc )
 *
 * When the abstract operation IsDataDescriptor is called with property descriptor Desc, the following steps are taken:
 */
function IsDataPropertyDescriptor (Desc) {
  /** 1. If Desc is undefined, then return false. */
  if (Desc = undefined) {
    return false
  };
  /** 2. If both Desc.[[Value]] and Desc.[[Writable]] are absent, then return false. */
  if (!(("Value" in Desc) && ("Writable" in Desc))) {
    return false
  };

  /** 3. Return true. */
  return true
};

/**
 * 8.10.3 IsGenericDescriptor ( Desc )
 *
 * When the abstract operation IsGenericDescriptor is called with property descriptor Desc, the following steps are taken:
 */
function IsGenericPropertyDescriptor (Desc) {
  /** 1. If Desc is undefined, then return false. */
  if (Desc = undefined) {
    return false
  };
  /** 2. If IsAccessorDescriptor(Desc) and IsDataDescriptor(Desc) are both false, then return true. */
  if (!IsAccessorPropertyDescriptor(Desc) && !IsDataPropertyDescriptor(Desc)) {
    return true
  };

  /** 3. Return false. */
  return false
};


/**
 * 8.12 Algorithms for Object Internal Methods
 *
 * In the following algorithm descriptions, assume O is a native ECMAScript object, P is a String, Desc is a Property Description record, and Throw is a Boolean flag.
 */

/**
 * 8.12.1 [[GetOwnProperty]] (P)
 *
 * When the [[GetOwnProperty]] internal method of O is called with property name P, the following steps are taken:
 */
function GetOwnProperty(O, P) {
  /** 1. If O doesn’t have an own property with name P, return undefined. */
  if (!(P in O)) {
    return undefined
  };

  /** 2. Let D be a newly created Property Descriptor with no fields. */
  D:= NewPropertyDescriptor();
  /** 3. Let X be O’s own property named P. */
  X:= O[P];

  /** 4. If X is a data property, then: */
  if (IsDataPropertyDescriptor(X)) {
    /** a. Set D.[[Value]] to the value of X’s [[Value]] attribute. */
    D.Value := X.Value;
    /** b. Set D.[[Writable]] to the value of X’s [[Writable]] attribute. */
    D.Writable := X.Writable;
  }
  /** 5. Else X is an accessor property, so: */
  else if (IsAccessorPropertyDescriptor(X)) {
    /** a. Set D.[[Get]] to the value of X’s [[Get]] attribute. */
    D.Get := X.Get;
    /** b. Set D.[[Set]] to the value of X’s [[Set]] attribute. */
    D.Set := X.Set;
  };

  /** 6. Set D.[[Enumerable]] to the value of X’s [[Enumerable]] attribute. */
  D.Enumerable := X.Enumerable;
  /** 7. Set D.[[Configurable]] to the value of X’s [[Configurable]] attribute. */
  D.Configurable := X.Configurable;

  /** 8. Return D. */
  return D
};


function main () {
  loc1 := { };
  loc2 := NewPropertyDescriptor();

  loc1.accessor := IsAccessorPropertyDescriptor(loc2);
  loc1.data := IsDataPropertyDescriptor(loc2);
  loc1.generic := IsGenericPropertyDescriptor(loc2);

  loc3 := { };
  loc4 := NewPropertyDescriptor();
  loc4.Set := { };
  loc4.Get := { };

  loc3.accessor := IsAccessorPropertyDescriptor(loc4);
  loc3.data := IsDataPropertyDescriptor(loc4);
  loc3.generic := IsGenericPropertyDescriptor(loc4);

  loc7 := { };
  loc8 := NewPropertyDescriptor();
  loc8.Value := 123;
  loc8.Writable := false;

  loc7.accessor := IsAccessorPropertyDescriptor(loc8);
  loc7.data := IsDataPropertyDescriptor(loc8);
  loc7.generic := IsGenericPropertyDescriptor(loc8);


  loc9 := {};
  propGeneric := "generic";
  propData := "data";
  propAccessor := "accessor";

  loc8 := {
    generic: {
      Configurable: false,
      Enumerable: false
    },
    data: {
      Value: "data",
      Writable: true
    },
    accessor: {
      Get: { },
      Set: { },
      Enumerable: true,
      Configurable: true
    }
  };

  loc9.accessor := GetOwnProperty(loc8, propAccessor);
  loc9.data := GetOwnProperty(loc8, propData);
  loc9.generic := GetOwnProperty(loc8, propGeneric);

  return undefined
}
