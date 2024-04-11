open! Core

type 'a node = {
  id : int;
  mutable value : 'a;
  mutable next : 'a node option;
  mutable prev : 'a node option;
}

type 'a t = {
  mutable length : int;
  mutable first : 'a node option;
  mutable last : 'a node option;
}
