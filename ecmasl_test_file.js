function NewObject () {
  return {}
};

function NewPropertyDescriptor() {
  return {}
};

function TypeErrorException() {
  return {
    exception: "TypeError"
  }
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
  if (!("Get" in Desc) && !("Set" in Desc)) {
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
  if (!("Value" in Desc) && !("Writable" in Desc)) {
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
  obj := NewObject();

  /** 3. If IsDataDescriptor(Desc) is true, then: */
  if (IsDataPropertyDescriptor(Desc)) {
    /** a. Call the [[DefineOwnProperty]] internal method of obj with arguments "value",
     *  Property Descriptor {[[Value]]: Desc.[[Value]], [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true}, and false. */
    DefineOwnProperty(obj, "value", {
      Value: Desc.Value,
      Writable: true,
      Enumerable: true,
      Configurable: true
    }, false);
    /** b. Call the [[DefineOwnProperty]] internal method of obj with arguments "writable",
     *  Property Descriptor {[[Value]]: Desc.[[Writable]], [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true}, and false. */
    DefineOwnProperty(obj, "writable", {
      Value: Desc.Writable,
      Writable: true,
      Enumerable: true,
      Configurable: true
    }, false);
  }
  /** 4. Else, IsAccessorDescriptor(Desc) must be true, so: */
  else {
    /** a. Call the [[DefineOwnProperty]] internal method of obj with arguments "get",
     *  Property Descriptor {[[Value]]: Desc.[[Get]], [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true}, and false. */
    DefineOwnProperty(obj, "get", {
      Value: Desc.Get,
      Writable: true,
      Enumerable: true,
      Configurable: true
    }, false);
    /** b. Call the [[DefineOwnProperty]] internal method of obj with arguments "set",
     *  Property Descriptor {[[Value]]: Desc.[[Set]], [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true}, and false. */
    DefineOwnProperty(obj, "set", {
      Value: Desc.Set,
      Writable: true,
      Enumerable: true,
      Configurable: true
    }, false);
  };

  /** 5. Call the [[DefineOwnProperty]] internal method of obj with arguments "enumerable",
   *  Property Descriptor {[[Value]]: Desc.[[Enumerable]], [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true}, and false. */
  DefineOwnProperty(obj, "enumerable", {
    Value: Desc.Enumerable,
    Writable: true,
    Enumerable: true,
    Configurable: true
  }, false);

  /** 6. Call the [[DefineOwnProperty]] internal method of obj with arguments "configurable",
   *    Property Descriptor {[[Value]]: Desc.[[Configurable]], [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true}, and false. */
  DefineOwnProperty(obj, "configurable", {
    Value: Desc.Configurable,
    Writable: true,
    Enumerable: true,
    Configurable: true
  }, false);

   /** 7. Return obj. */
   return obj
};

/**
 * 8.10.5 ToPropertyDescriptor ( Obj )
 *
 * When the abstract operation ToPropertyDescriptor is called with object Desc, the following steps are taken:
 */
function ToPropertyDescriptor (Obj) {
  /** 1. If Type(Obj) is not Object throw a TypeError exception. */
  if (!(Type(Obj) = Object)) {
    return TypeErrorException();
  };

  /** 2. Let desc be the result of creating a new Property Descriptor that initially has no fields. */
  desc := NewPropertyDescriptor();

  /** 3. If the result of calling the [[HasProperty]] internal method of Obj with argument "enumerable" is true, then: */
  if (HasProperty(Obj, "enumerable")) {
    /** a. Let enum be the result of calling the [[Get]] internal method of Obj with "enumerable". */
    enum := Get(Obj, "enumerable");
    /** b. Set the [[Enumerable]] field of desc to ToBoolean(enum). */
    desc.Enumerable := ToBoolean(enum);
  };

  /** 4. If the result of calling the [[HasProperty]] internal method of Obj with argument "configurable" is true, then: */
  if (HasProperty(Obj, "configurable")) {
    /** a. Let conf be the result of calling the [[Get]] internal method of Obj with argument "configurable". */
    conf:= Get(Obj, "configurable");
    /** b. Set the [[Configurable]] field of desc to ToBoolean(conf). */
    desc.Configurable := ToBoolean(conf);
  };

  /** 5. If the result of calling the [[HasProperty]] internal method of Obj with argument "value" is true, then: */
  if (HasProperty(Obj, "value")) {
    /** a. Let value be the result of calling the [[Get]] internal method of Obj with argument “value”. */
    value := Get(Obj, "value");
    /** b. Set the [[Value]] field of desc to value. */
    desc.Value := value;
  };

  /** 6. If the result of calling the [[HasProperty]] internal method of Obj with argument "writable" is true, then: */
  if (HasProperty(Obj, "writable")) {
    /** a. Let writable be the result of calling the [[Get]] internal method of Obj with argument "writable". */
    writable := Get(Obj, "writable");
    /** b. Set the [[Writable]] field of desc to ToBoolean(writable). */
    desc.Writable := ToBoolean(writable);
  };

  /** 7. If the result of calling the [[HasProperty]] internal method of Obj with argument "get" is true, then: */
  if (HasProperty(Obj, "get")) {
    /** a. Let getter be the result of calling the [[Get]] internal method of Obj with argument "get". */
    getter := Get(Obj, "get");
    /** b. If IsCallable(getter) is false and getter is not undefined, then throw a TypeError exception. */
    if (IsCallable(getter) = false && !(getter = undefined)) {
      return TypeErrorException();
    };
    /** c. Set the [[Get]] field of desc to getter. */
    desc.Get := getter;
  };

  /** 8. If the result of calling the [[HasProperty]] internal method of Obj with argument "set" is true, then: */
  if (HasProperty(Obj, "set")) {
    /** a. Let setter be the result of calling the [[Get]] internal method of Obj with argument "set". */
    setter := Get(Obj, "set");
    /** b. If IsCallable(setter) is false and setter is not undefined, then throw a TypeError exception. */
    if (IsCallable(setter) = false && !(setter = undefined)) {
      return TypeErrorException();
    };
    /** c. Set the [[Set]] field of desc to setter. */
    desc.Set := setter;
  };

  /** 9. If either desc.[[Get]] or desc.[[Set]] are present, then: */
  if (("Get" in desc) || ("Set" in desc)) {
    /** a. If either desc.[[Value]] or desc.[[Writable]] are present, then throw a TypeError exception. */
    if (("Value" in desc) || ("Writable" in desc)) {
      return TypeErrorException();
    }
  };

  /** 10. Return desc. */
  return desc
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
  D := NewPropertyDescriptor();
  /** 3. Let X be O’s own property named P. */
  X := O[P];

  /** 4. If X is a data property, then: */
  if (IsDataPropertyDescriptor(X)) {
    /** a. Set D.[[Value]] to the value of X’s [[Value]] attribute. */
    D.Value := X.Value;
    /** b. Set D.[[Writable]] to the value of X’s [[Writable]] attribute. */
    D.Writable := X.Writable;
  }
  /** 5. Else X is an accessor property, so: */
  else {
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
  prop := GetOwnProperty(O, P);

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
  return GetProperty(proto, P)
};

/**
 * 8.12.3 [[Get]] (P)
 *
 * When the [[Get]] internal method of O is called with property name P, the following steps are taken:
 */
function Get(O, P) {
  /** 1. Let desc be the result of calling the [[GetProperty]] internal method of O with property name P. */
  desc := GetProperty(O, P);

  /** 2. If desc is undefined, return undefined. */
  if (desc = undefined) {
    return undefined;
  };

  /** 3. If IsDataDescriptor(desc) is true, return desc.[[Value]]. */
  if (IsDataPropertyDescriptor(desc)) {
    return desc.Value
  }
  /** 4. Otherwise, IsAccessorDescriptor(desc) must be true so, let getter be desc.[[Get]]. */
  else {
    getter := desc.Get;

    /** 5. If getter is undefined, return undefined. */
    if (getter = undefined) {
      return undefined
    };

    /** 6. Return the result calling the[[Call]] internal method of getter providing O as the this value and providing no arguments. */
    return Call(getter, O)
  }
};

/**
 * 8.12.4 [[CanPut]] (P)
 *
 * When the [[CanPut]] internal method of O is called with property name P, the following steps are taken:
 */
function CanPut(O, P) {
  /** 1. Let desc be the result of calling the[[GetOwnProperty]] internal method of O with argument P. */
  desc := GetOwnProperty(O, P);

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
    else {
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
  inherited := GetProperty(proto, P);

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
   *  If possible, host objects should not allow [[Put]] operations in situations where this definition of [[CanPut]] returns false. */
};

/**
 * 8.12.5 [[Put]] ( P, V, Throw )
 *
 * When the [[Put]] internal method of O is called with property P, value V, and Boolean flag Throw, the following steps are taken:
 */
function Put (O, P, V, Throw) {
  /** 1. If the result of calling the [[CanPut]] internal method of O with argument P is false, then: */
  if (CanPut(O, P) = false) {
    /** a. If Throw is true, then throw a TypeError exception. */
    if (Throw) {
      return TypeErrorException();
    }
    /** b. Else return. */
    else {
      return
    };
  };
  /** 2. Let ownDesc be the result of calling the [[GetOwnProperty]] internal method of O with argument P. */
  ownDesc := GetOwnProperty(O, P);

  /** 3. If IsDataDescriptor(ownDesc) is true, then: */
  if (IsDataPropertyDescriptor(ownDesc)) {
    /** a. Let valueDesc be the Property Descriptor {[[Value]]: V}. */
    valueDesc := {
      Value: V
    };
    /** b. Call the [[DefineOwnProperty]] internal method of O passing P, valueDesc, and Throw as arguments. */
    DefineOwnProperty(O, P, valueDesc, Throw);

    /** c. Return. */
    return
  };

  /** 4. Let desc be the result of calling the [[GetProperty]] internal method of O with argument P.
   *     This may be either an own or inherited accessor property descriptor or an inherited data property descriptor. */
  desc := GetProperty(O, P);

  /** 5. If IsAccessorDescriptor(desc) is true, then: */
  if (IsAccessorPropertyDescriptor(desc)) {
    /** a. Let setter be desc.[[Set]] which cannot be undefined. */
    setter := desc.Set;

    /** b. Call the [[Call]] internal method of setter providing O as the this value and providing V as the sole argument. */
    Call(setter, O, V);
  }
  /** 6. Else, create a named data property named P on object O as follows */
  else {
    /** a. Let newDesc be the Property Descriptor {[[Value]]: V, [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true}. */
    newDesc := {
      Value: V,
      Writable: true,
      Enumerable: true,
      Configurable: true
    };

    /** b. Call the [[DefineOwnProperty]] internal method of O passing P, newDesc, and Throw as arguments. */
    DefineOwnProperty(O, P, newDesc, Throw);
  };

  /** 7. Return. */
  return
};

/**
 * 8.12.6 [[HasProperty]] (P)
 *
 * When the [[HasProperty]] internal method of O is called with property name P, the following steps are taken:
 */
function HasProperty (O, P) {
  /** 1. Let desc be the result of calling the [[GetProperty]] internal method of O with property name P. */
  desc := GetProperty(O, P);

  /** 2. If desc is undefined, then return false. */
  if (desc = undefined) {
    return false
  }
  /** 3. Else return true. */
  else {
    return true
  };
};

/**
 * 8.12.7 [[Delete]] (P, Throw)
 *
 * When the [[Delete]] internal method of O is called with property name P and the Boolean flag Throw, the following steps are taken:
 */
function Delete (O, P, Throw) {
  /** 1. Let desc be the result of calling the [[GetOwnProperty]] internal method of O with property name P. */
  desc := GetOwnProperty(O, P);

  /** 2. If desc is undefined, then return true. */
  if (desc = undefined) {
    return true
  };

  /** 3. If desc.[[Configurable]] is true, then */
  if (desc.Configurable = true) {
    /** a. Remove the own property with name P from O. */
    delete O[P];

    /** b. Return true. */
    return true
  }
  /** 4. Else if Throw, then throw a TypeError exception. */
  else if (Throw) {
    return TypeErrorException()
  };

  /** 5. Return false. */
  return false
};

/**
 * 8.12.8 [[DefaultValue]] (hint)
 */
function DefaultValue (O, hint) {
  /** When the [[DefaultValue]] internal method of O is called with hint String, the following steps are taken: */
  if (typeof hint = __$string) {
    /** 1. Let toString be the result of calling the [[Get]] internal method of object O with argument "toString". */
    toString := Get(O, "toString");

    /** 2. If IsCallable(toString) is true then: */
    if (IsCallable(toString)) {
      /** a. Let str be the result of calling the [[Call]] internal method of toString, with O as the this value and an empty argument list. */
      str := Call(toString, O, []);

      /** b. If str is a primitive value, return str. */
      if (IsPrimitiveValue(str)) {
        return str
      };
    };

    /** 3. Let valueOf be the result of calling the [[Get]] internal method of object O with argument "valueOf". */
    valueOf := Get(O, "valueOf");

    /** 4. If IsCallable(valueOf) is true then: */
    if (IsCallable(valueOf)) {
      /** a. Let val be the result of calling the [[Call]] internal method of valueOf, with O as the this value and an empty argument list. */
      val := Call(valueOf, O, []);

      /** b. If val is a primitive value, return val. */
      if (IsPrimitiveValue(val)) {
        return val
      }
    };

    /** 5. Throw a TypeError exception. */
    return TypeErrorException()
  }
  /** When the [[DefaultValue]] internal method of O is called with hint Number, the following steps are taken: */
  else if (typeof hint = __$int || typeof hint = __$float || hint = undefined) {
    /** 1. Let valueOf be the result of calling the [[Get]] internal method of object O with argument "valueOf". */
    valueOf := Get(O, "valueOf");

    /** 2. If IsCallable(valueOf) is true then: */
    if (IsCallable(valueOf)) {
      /** a. Let val be the result of calling the [[Call]] internal method of valueOf, with O as the this value and an empty argument list. */
      val := Call(valueOf, O, []);

      /** b. If val is a primitive value, return val. */
      if (IsPrimitiveValue(val)) {
        return val
      };
    };

    /** 3. Let toString be the result of calling the [[Get]] internal method of object O with argument "toString". */
    toString := Get(O, "toString");

    /** 4. If IsCallable(toString) is true then: */
    if (IsCallable(toString)) {
      /** a. Let str be the result of calling the [[Call]] internal method of toString, with O as the this value and an empty argument list. */
      str := Call(toString, O, []);

      /** b. If str is a primitive value, return str. */
      if (IsPrimitiveValue(str)) {
        return str
      };
    };

    /** 5. Throw a TypeError exception. */
    return TypeErrorException()
  };

/** When the [[DefaultValue]] internal method of O is called with no hint, then it behaves as if the hint were Number,
 * unless O is a Date object (see 15.9.6), in which case it behaves as if the hint were String.
 *
 * The above specification of [[DefaultValue]] for native objects can return only primitive values.
 * If a host object implements its own [[DefaultValue]] internal method,
 * it must ensure that its [[DefaultValue]] internal method can return only primitive values.
 */
};


function main () {
  /* Test Property Descriptor type and respective operations (see 8.10). */
  testPropertyDescriptor();

  /* Test Object internal methods (see 8.12) */
  testObjectInternalMethods();

  return
};

function testPropertyDescriptor() {
  loc1 := {};
  loc2 := NewPropertyDescriptor();

  /* loc1 must be generic */
  loc1.accessor := IsAccessorPropertyDescriptor(loc2);
  loc1.data     := IsDataPropertyDescriptor(loc2);
  loc1.generic  := IsGenericPropertyDescriptor(loc2);

  loc3 := {};
  loc4 := NewPropertyDescriptor();
  loc4.Set := { };
  loc4.Get := { };

  /* loc3 must be accessor */
  loc3.accessor := IsAccessorPropertyDescriptor(loc4);
  loc3.data     := IsDataPropertyDescriptor(loc4);
  loc3.generic  := IsGenericPropertyDescriptor(loc4);

  loc7 := {};
  loc8 := NewPropertyDescriptor();
  loc8.Value := 123;
  loc8.Writable := false;

  /* loc7 must be data */
  loc7.accessor := IsAccessorPropertyDescriptor(loc8);
  loc7.data     := IsDataPropertyDescriptor(loc8);
  loc7.generic  := IsGenericPropertyDescriptor(loc8);

  return
};

function testObjectInternalMethods() {
  loc9  := {};
  loc14 := {
    dataProp: {
      Value: "data",
      Writable: true,
      Configurable: false,
      Enumerable: false
    },
    accessorProp: {
      Get: {},
      Set: {},
      Enumerable: true,
      Configurable: true
    }
  };

  loc9.accessor := GetOwnProperty(loc14, "accessorProp");
  loc9.data     := GetOwnProperty(loc14, "dataProp");

  return
}
