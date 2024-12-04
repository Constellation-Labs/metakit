package io.constellationnetwork.metagraph_sdk

package object syntax {
  object l0Context extends L0NodeContextSyntax
  object l1Context extends L1NodeContextSyntax
  object cis extends CurrencyIncrementalSnapshotSyntax

  object all extends L0NodeContextSyntax with L1NodeContextSyntax with CurrencyIncrementalSnapshotSyntax
}
