package lifecycle.committed

import io.constellationnetwork.metagraph_sdk.crypto.smt.SparseMerkleRoot
import io.constellationnetwork.metagraph_sdk.lifecycle.committed.{CommitKey, CommittedBreadcrumb, CommittedRoots}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.hash.Hash

import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.EncoderOps
import weaver.SimpleIOSuite

/**
 * Wire-format KATs for the committed-state breadcrumb codecs — the constant-size on-chain commitment
 * a syncing node trusts. Pins the exact ordered field-name list + round-trip so a rename can't
 * silently change what a node decodes from a signed snapshot. (CommittedRoots' `combinedHash` is over
 * RAW bytes, independent of this circe codec, so it is unaffected; this guards the transport/storage
 * encoding.) CommitKey is a validated string newtype (encodes as a bare string, not an object).
 */
object CommittedRootsCodecKatSuite extends SimpleIOSuite {

  private val mptRoot: Hash = Hash("aa" * 32)
  private val catalogRoot: SparseMerkleRoot = SparseMerkleRoot(Hash("bb" * 32))

  private def keys(j: Json): List[String] = j.asObject.toList.flatMap(_.keys.toList)

  pureTest("CommitKey encodes as a bare string (validated newtype), round-trips") {
    val k = CommitKey.unsafe("fiber/abc-1")
    expect(k.asJson == Json.fromString("fiber/abc-1"))
      .and(expect(decode[CommitKey](k.asJson.noSpaces) == Right(k)))
  }

  pureTest("CommittedRoots wire keys = [mptRoot, catalogRoot]; round-trips") {
    val r = CommittedRoots(mptRoot, catalogRoot)
    expect(keys(r.asJson) == List("mptRoot", "catalogRoot"))
      .and(expect(decode[CommittedRoots](r.asJson.noSpaces) == Right(r)))
  }

  pureTest("CommittedBreadcrumb wire keys = [ordinal, roots]; round-trips") {
    val b = CommittedBreadcrumb(SnapshotOrdinal.MinValue, CommittedRoots(mptRoot, catalogRoot))
    expect(keys(b.asJson) == List("ordinal", "roots"))
      .and(expect(decode[CommittedBreadcrumb](b.asJson.noSpaces) == Right(b)))
  }
}
