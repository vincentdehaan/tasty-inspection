package nl.vindh.tastyinspection.example

import com.softwaremill.tagging.{@@, Tagger}

package object domain {
  sealed trait ShoeSizeTag
  sealed trait ShirtSizeTag

  type ShoeSize = Int @@ ShoeSizeTag
  type ShirtSize = String @@ ShirtSizeTag
  type StringAlias = String
}
