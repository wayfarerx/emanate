package net.wayfarerx.oversite
package model

class Search {

}

object Search {

  trait Indexed {

    type IndexedType >: this.type <: Indexed


  }

}