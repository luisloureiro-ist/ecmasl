function GetOwnProperty (O, P) {
  /* 1. If O doesn’t have an own property with name P, return undefined. */
  if (!(P in O)) {
    return undefined
  };

  /*
    2.
  */
  D := NewPropertyDescriptor();
  X := O[P];

  if (IsDataPropertyDescriptor(X)) {
    D.Value := X.Value;
    D.Writable := X.Writable;
  } else if (IsAccessorPropertyDescriptor(X)) {
    D.Get := X.Get;
    D.Set := X.Set;
  };

  D.Enumerable := X.Enumerable;
  D.Configurable := X.Configurable;

  return D
};

function NewPropertyDescriptor () {
  return {}
};

function IsAccessorPropertyDescriptor (Desc) {
  if (Desc = undefined) {
    return false
  };
  if (!(("Get" in Desc) && ("Set" in Desc))) {
    return false
  };

  return true
};

function IsDataPropertyDescriptor (Desc) {
  if (Desc = undefined) {
    return false
  };
  if (!(("Value" in Desc) && ("Writable" in Desc))) {
    return false
  };

  return true
};

function IsGenericPropertyDescriptor (Desc) {
  if (Desc = undefined) {
    return false
  };
  if (!IsAccessorPropertyDescriptor(Desc) && !IsDataPropertyDescriptor(Desc)) {
    return true
  };

  return false
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
