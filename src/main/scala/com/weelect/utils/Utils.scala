package com.weelect.utils

import org.bitcoinj.core._

import scala.collection.JavaConverters._
/**
 * Created by t on 1/28/16.
 */
object Utils {

  type witnessAddr = String
  type coordAddr = String
  type codeHash = String

  def createInitString(ca:coordAddr, wa:witnessAddr, hash:codeHash, otherOutputAddrs:Option[List[String]]) = {
    val subStringsWit = wa.sliding(5).toSet
    val subStringsCoo = ca.sliding(5).toSet

    val others = otherOutputAddrs.getOrElse(List.empty)

    val witnessAddrPrefix = subStringsWit.filter(s=> !subStringsCoo.contains(s) && !others.exists(_.contains(s))).head
    val coordinationAddrPrefix = subStringsCoo.filter(s=> !subStringsWit.contains(s) && !others.exists(_.contains(s))).head

    require(!wa.contains(coordinationAddrPrefix) && !ca.contains(witnessAddrPrefix) && !others.exists(_.contains(witnessAddrPrefix)) && !others.exists(_.contains(coordinationAddrPrefix)), "distinct substring problem")

    val initCoordString = "i:" + coordinationAddrPrefix
    val witnessAddrString = ",w:" + witnessAddrPrefix
    val hashString = ",h:" + hash

    val op_returnString =  initCoordString + witnessAddrString + hashString

    op_returnString.take(38)
  }

  def parseCoordinationTx(base58Addrs: List[String], initString: String): Option[(coordAddr, witnessAddr, codeHash)] = {
    try {
      val map: Map[String, String] = initString.split(",").map { a => val s = a.split(":"); s(0) -> s(1) }.toMap
      val witnessSubStr = map("w")
      val coordSubStr = map("i")
      val hashSubStr = map("h")

      val witnessAddr = base58Addrs.filter(_.contains(witnessSubStr))
      val coordAddr = base58Addrs.filter(_.contains(coordSubStr))

      require(witnessAddr.length == 1 && coordAddr.length == 1, "parse error-- multiple matching substrings!")

      Some((coordAddr(0), witnessAddr(0), hashSubStr))
    }
    catch{
      case e:Exception=> e.printStackTrace(); None
    }
  }

  def getOpReturnTXs(txs:List[Transaction]):List[(Transaction, String)] ={
    val returns = txs.map{tx=>
      tx.getOutputs.asScala
        .filter(_.getScriptPubKey.isOpReturn)
        .map(scr=>tx->new String(scr.getScriptPubKey.getChunks.get(1).data))
    }
    returns.flatten
  }

  //TODO:TEST THIS!!!
  def passedThrough(params: NetworkParameters,t:Transaction, a:String): Boolean = {
    //--[a]--------(t)--->[doesn't matter]
    //--[y]-----|
    //--[z]-----/
    //the percent of the inputs of this transaction which came from outputs to a
    val ins = t.getInputs.asScala

    ins.exists{in=>in.getScriptSig.getFromAddress(params).toBase58 == a}
//    ins.map(in=>in.getParentTransaction.getOutputs.asScala.toList.map(o=>if(o.getAddressFromP2PKHScript(params).toBase58 != null && o.getAddressFromP2PKHScript(params).toBase58 == a.toBase58) o.getValue else Coin.ZERO)).flatten.map(_.getValue).sum > 0
  }

}

object ParseTest extends App {
  println(Utils.parseCoordinationTx(List("1L1BWWCqPRWYYaHDwTj4genaYUtFpXC5uq", "1DXqsqqLh5Bej6pKrXw9aQxWXoEC5Qrbnw", "14rQHjHEcjNLd2mmq1jBTrJ9yVZ2xS6kax"), "i:j6pKr,w:xS6ka,h:1234567"))
}