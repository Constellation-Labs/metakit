package io.constellationnetwork.metagraph_sdk.crypto.smt.api

import io.constellationnetwork.metagraph_sdk.crypto.smt.{SparseMerkleProof, SparseMerkleProofError}
import io.constellationnetwork.security.hex.Hex

/**
 * Produces a native [[SparseMerkleProof]] for a key against a [[io.constellationnetwork.metagraph_sdk.crypto.smt.SparseMerkleTree]]
 * -- INCLUSION when the key is present, ABSENCE when it is not (the caller does not pre-decide which; absence is
 * first-class).
 *
 * Mirrors the `MerklePatricia*Prover` split: the prover walks the tree and emits the authentication path; verification
 * (and the mandatory value-binding) is the [[SparseMerkleVerifier]]'s job.
 */
trait SparseMerkleProver[F[_]] {

  /**
   * Prove `key`'s status against the tree this prover was built for. `Right` carries an [[SparseMerkleProof.Inclusion]] or
   * [[SparseMerkleProof.Absence]]; `Left` carries a structural [[SparseMerkleProofError]] (the in-memory prover does not fail under normal
   * operation, but the signature keeps the error channel uniform with [[SparseMerkleVerifier]]).
   */
  def prove(key: Hex): F[Either[SparseMerkleProofError, SparseMerkleProof]]
}

object SparseMerkleProver {
  def apply[F[_]](implicit prover: SparseMerkleProver[F]): SparseMerkleProver[F] = prover

  /** Provides syntax extensions for more ergonomic proof generation. */
  object syntax {

    implicit class SparseMerkleKeyOps(private val key: Hex) extends AnyVal {

      /** Prove this key's status (inclusion or absence) against the tree the prover was built for. */
      def attest[F[_]](implicit P: SparseMerkleProver[F]): F[Either[SparseMerkleProofError, SparseMerkleProof]] =
        P.prove(key)
    }
  }
}
