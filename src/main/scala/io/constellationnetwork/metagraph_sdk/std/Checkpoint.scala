package io.constellationnetwork.metagraph_sdk.std

import io.constellationnetwork.schema.SnapshotOrdinal

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive

@derive(decoder, encoder)
case class Checkpoint[S](ordinal: SnapshotOrdinal, state: S)

object Checkpoint {
  def genesis[S](state: S): Checkpoint[S] = Checkpoint(SnapshotOrdinal.MinValue, state)
}
