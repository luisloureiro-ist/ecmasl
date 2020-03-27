
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
 * 8.10.4 FromPropertyDescriptor ( Desc )
 *
 * When the abstract operation FromPropertyDescriptor is called with property descriptor Desc, the following steps are taken:
 *
 * The following algorithm assumes that Desc is a fully populated Property Descriptor, such as that returned from [[GetOwnProperty]] (see 8.12.1).
 */
function FromPropertyDescriptor (Desc) {
  /** 1. If Desc is undefined, then return undefined. */
  if (Desc = undefined) {
    return undefined
  };

  /** 2. Let obj be the result of creating a new object as if by the expression new Object() where Object is the standard built-in constructor with that name. */
  obj := new Object();

  /** 3. If IsDataDescriptor(Desc) is true, then: */
  if (IsDataPropertyDescriptor(Desc)) {
    /** a. Call the [[DefineOwnProperty]] internal method of obj with arguments "value",
     *  Property Descriptor {[[Value]]: Desc.[[Value]], [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true}, and false. */
    obj.DefineOwnProperty("value", {
      Value: Desc.Value,
      Writable: true,
      Enumerable: true,
      Configurable: true
    }, false);
    /** b. Call the [[DefineOwnProperty]] internal method of obj with arguments "writable",
     *  Property Descriptor {[[Value]]: Desc.[[Writable]], [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true}, and false. */
    obj.DefineOwnProperty("writable", {
      Value: Desc.Writable,
      Writable: true,
      Enumerable: true,
      Configurable: true
    }, false);
  }
  /** 4. Else, IsAccessorDescriptor(Desc) must be true, so: */
  else if (IsAccessorPropertyDescriptor(Desc)) {
    /** a. Call the [[DefineOwnProperty]] internal method of obj with arguments "get",
     *  Property Descriptor {[[Value]]: Desc.[[Get]], [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true}, and false. */
    obj.DefineOwnProperty("get", {
      Value: Desc.Get,
      Writable: true,
      Enumerable: true,
      Configurable: true
    }, false);
    /** b. Call the [[DefineOwnProperty]] internal method of obj with arguments "set",
     *  Property Descriptor {[[Value]]: Desc.[[Set]], [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true}, and false. */
    obj.DefineOwnProperty("set", {
      Value: Desc.Set,
      Writable: true,
      Enumerable: true,
      Configurable: true
    }, false);
  };

  /** 5. Call the [[DefineOwnProperty]] internal method of obj with arguments "enumerable",
   *  Property Descriptor {[[Value]]: Desc.[[Enumerable]], [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true}, and false. */
  obj.DefineOwnProperty("enumerable", {
    Value: Desc.Enumerable,
    Writable: true,
    Enumerable: true,
    Configurable: true
  }, false);

  /** 6. Call the [[DefineOwnProperty]] internal method of obj with arguments "configurable",
   *    Property Descriptor {[[Value]]: Desc.[[Configurable]], [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true}, and false. */
  obj.DefineOwnProperty("configurable", {
    Value: Desc.Configurable,
    Writable: true,
    Enumerable: true,
    Configurable: true
  }, false);

   /** 7. Return obj. */
   return obj
}


/**
 * 8.10.5 ToPropertyDescriptor ( Obj )
 *
 * When the abstract operation ToPropertyDescriptor is called with object Desc, the following steps are taken:
 */
function ToPropertyDescriptor (Obj) {
  /** 1. If Type(Obj) is not Object throw a TypeError exception. */
  if (Type(Obj) != Object) {
    throw TypeErrorException();
  };

  /** 2. Let desc be the result of creating a new Property Descriptor that initially has no fields. */
  desc := NewPropertyDescriptor();

  /** 3. If the result of calling the [[HasProperty]] internal method of Obj with argument "enumerable" is true, then: */
  if (Obj.HasProperty("enumerable")) {
    /** a. Let enum be the result of calling the [[Get]] internal method of Obj with "enumerable". */
    enum := Obj.Get("enumerable");
    /** b. Set the [[Enumerable]] field of desc to ToBoolean(enum). */
    desc.Enumerable := ToBoolean(enum);
  };

  /** 4. If the result of calling the [[HasProperty]] internal method of Obj with argument "configurable" is true, then: */
  if (Obj.HasProperty("configurable")) {
    /** a. Let conf be the result of calling the [[Get]] internal method of Obj with argument "configurable". */
    conf := Obj.Get("configurable");
    /** b. Set the [[Configurable]] field of desc to ToBoolean(conf). */
    desc.Configurable := ToBoolean(conf);
  };

  /** 5. If the result of calling the [[HasProperty]] internal method of Obj with argument "value" is true, then: */
  if (Obj.HasProperty("value")) {
    /** a. Let value be the result of calling the [[Get]] internal method of Obj with argument “value”. */
    value := Obj.Get("value");
    /** b. Set the [[Value]] field of desc to value. */
    desc.Value := value;
  };

  /** 6. If the result of calling the [[HasProperty]] internal method of Obj with argument "writable" is true, then: */
  if (Obj.HasProperty("writable")) {
    /** a. Let writable be the result of calling the [[Get]] internal method of Obj with argument "writable". */
    writable := Obj.Get("writable");
    /** b. Set the [[Writable]] field of desc to ToBoolean(writable). */
    desc.Writable := ToBoolean(writable);
  };

  /** 7. If the result of calling the [[HasProperty]] internal method of Obj with argument "get" is true, then: */
  if (Obj.HasProperty("get")) {
    /** a. Let getter be the result of calling the [[Get]] internal method of Obj with argument "get". */
    getter := Obj.Get("get");
    /** b. If IsCallable(getter) is false and getter is not undefined, then throw a TypeError exception. */
    if (IsCallable(getter) = false && !(getter = undefined)) {
      throw TypeErrorException();
    };
    /** c. Set the [[Get]] field of desc to getter. */
    desc.Get := getter;
  };

  /** 8. If the result of calling the [[HasProperty]] internal method of Obj with argument "set" is true, then: */
  if (Obj.HasProperty("set")) {
    /** a. Let setter be the result of calling the [[Get]] internal method of Obj with argument "set". */
    setter := Obj.Get("set");
    /** b. If IsCallable(setter) is false and setter is not undefined, then throw a TypeError exception. */
    if (IsCallable(setter) = false && !(setter = undefined)) {
      throw TypeErrorException();
    }
    /** c. Set the [[Set]] field of desc to setter. */
    desc.Set := setter;
  };

  /** 9. If either desc.[[Get]] or desc.[[Set]] are present, then: */
  if ("Get" in desc || "Set" in desc) {
    /** a. If either desc.[[Value]] or desc.[[Writable]] are present, then throw a TypeError exception. */
    if ("Value" in desc || "Writable" in desc) {
      throw TypeErrorException();
    }
  };

  /** 10. Return desc. */
  return desc
}

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

/**
 * 8.12.2 [[GetProperty]] (P)
 *
 * When the [[GetProperty]] internal method of O is called with property name P, the following steps are taken:
 */
function GetProperty (O, P) {
  /** 1. Let prop be the result of calling the [[GetOwnProperty]] internal method of O with property name P. */
  prop := O.GetOwnProperty(P);

  /** 2. If prop is not undefined, return prop. */
  if (!(prop = undefined)) {
    return prop
  };

  /** 3. Let proto be the value of the [[Prototype]] internal property of O. */
  proto := O.Prototype;

  /** 4. If proto is null, return undefined. */
  if (proto = null) {
    return undefined
  };

  /** 5. Return the result of calling the [[GetProperty]] internal method of proto with argument P. */
  return proto.GetProperty(P)
};

/**
 * 8.12.3 [[Get]] (P)
 *
 * When the [[Get]] internal method of O is called with property name P, the following steps are taken:
 */
function Get(O, P) {
  /** 1. Let desc be the result of calling the [[GetProperty]] internal method of O with property name P. */
  desc := O.GetProperty(P);

  /** 2. If desc is undefined, return undefined. */
  if (desc = undefined) {
    return undefined;
  };

  /** 3. If IsDataDescriptor(desc) is true, return desc.[[Value]]. */
  if (IsDataPropertyDescriptor(desc)) {
    return desc.Value
  }
  /** 4. Otherwise, IsAccessorDescriptor(desc) must be true so, let getter be desc.[[Get]]. */
  else if (IsAccessorPropertyDescriptor(desc)) {
    getter := desc.Get;

    /** 5. If getter is undefined, return undefined. */
    if (getter = undefined) {
      return undefined
    };

    /** 6. Return the result calling the[[Call]] internal method of getter providing O as the this value and providing no arguments. */
    return getter.Call(O)
  }
};

/**
 * 8.12.4 [[CanPut]] (P)
 *
 * When the [[CanPut]] internal method of O is called with property name P, the following steps are taken:
 */
function CanPut(O, P) {
  /** 1. Let desc be the result of calling the[[GetOwnProperty]] internal method of O with argument P. */
  desc := O.GetOwnProperty(P);

  /** 2. If desc is not undefined, then: */
  if (!(desc = undefined)) {
    /** a. If IsAccessorDescriptor(desc) is true, then: */
    if (IsAccessorPropertyDescriptor(desc)) {
      /** i. If desc.[[Set]] is undefined, then return false. */
      if (desc.Set = undefined) {
        return false
      }
      /** ii. Else return true. */
      else {
        return true
      };
    }
    /** b. Else, desc must be a DataDescriptor so return the value of desc.[[Writable]]. */
    else if (IsDataPropertyDescriptor(desc)) {
      return desc.Writable
    };
  };
  /** 3. Let proto be the [[Prototype]] internal property of O. */
  proto := O.Prototype;

  /** 4. If proto is null, then return the value of the [[Extensible]] internal property of O. */
  if (proto = null) {
    return O.Extensible;
  };

  /** 5. Let inherited be the result of calling the [[GetProperty]] internal method of proto with property name P. */
  inherited := proto.GetProperty(P);

  /** 6. If inherited is undefined, return the value of the [[Extensible]] internal property of O. */
  if (inherited = undefined) {
    return O.Extensible
  };

  /** 7. If IsAccessorDescriptor(inherited) is true, then */
  if (IsAccessorPropertyDescriptor(inherited)) {
    /** a. If inherited.[[Set]] is undefined, then return false. */
    if (inherited.Set = undefined) {
      return false
    }
    /** b. Else return true. */
    else {
      return true
    };
  }
  /** 8. Else, inherited must be a DataDescriptor */
  else if (IsDataPropertyDescriptor(inherited)) {
    /** a. If the [[Extensible]] internal property of O is false, return false. */
    if (O.Extensible = false) {
      return false
    }
    /** b. Else return the value of inherited.[[Writable]]. */
    else {
      return inherited.Writable
    }
  }
  /** Host objects may define additional constraints upon [[Put]] operations.
   * If possible, host objects should not allow [[Put]] operations in situations where this definition of [[CanPut]] returns false. */
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
