package com.weelect.audit

import java.io.File
import java.util.Date

import com.weelect.witness.Witness
import Witness._
import com.weelect.utils
import com.weelect.utils.Utils
import org.bitcoinj.core.{Address, BlockChain, PeerGroup, Wallet}
import org.bitcoinj.net.discovery.DnsDiscovery
import org.bitcoinj.params.MainNetParams
import org.bitcoinj.store.SPVBlockStore
import org.bitcoinj.utils.BriefLogFormatter

import scala.collection.JavaConverters._
/**
 * Created by t on 2/2/16.
 */

class ElectionState(val coordinationAddr:String,
                    val witnessAddr:String,
                    val hash:String){

  val walletFile = new java.io.File(coordinationAddr + witnessAddr + ".electionStateWallet")

  BriefLogFormatter.initVerbose()
  val params = MainNetParams.get()
  val wallet = try {
    Wallet.loadFromFile(walletFile)
  }
  catch {
    case _ => new Wallet(params)
  }

  val store = new SPVBlockStore(params, new File(coordinationAddr + witnessAddr + ".electionStateSPVChain"))
  //new MemoryBlockStore(params)//
  val chain = new BlockChain(params, wallet, store);
  val peerGroup = new PeerGroup(params, chain);
  peerGroup.addPeerDiscovery(new DnsDiscovery(params))
  val t0 = System.currentTimeMillis() / 1000 - 24 * 60 * 60 * 2 * 7
  wallet.addWatchedAddress(Address.fromBase58(params, coordinationAddr), t0)
  wallet.addWatchedAddress(Address.fromBase58(params, witnessAddr), t0)
  peerGroup.addWallet(wallet);
  peerGroup.start()
  peerGroup.downloadBlockChain()
  wallet.saveToFile(walletFile)

  //todo: figure this out.
  val endTime = new Date((t0 + 60*60*24*7*5*10) * 1000)

  val txs = wallet.getWalletTransactions.iterator().asScala.toList.map(_.getTransaction).filter(_.getUpdateTime.before(endTime))

  val nTX = txs.length

  val op_returns = Utils.getOpReturnTXs(txs)
  //  println("found op_returns:\n  " + op_returns.map(_._2).mkString("\n  "))

  val initString = utils.Utils.createInitString(coordinationAddr, witnessAddr, hash, None)
  //  println("looking for init string: " + initString)

  val initOPReturn = {
    val initOPReturns = op_returns.filter(_._2 == initString)
    if(initOPReturns.length > 0){
      //the first one.
      Some(initOPReturns.minBy(_._1.getUpdateTime))
    }
    else None
  }

  println(op_returns.map(_._2).mkString("\n"))

  val coordinationOutput = initOPReturn.map(_._1.getOutputs.asScala.find(o => o.getScriptPubKey.isSentToAddress && o.getAddressFromP2PKHScript(params).toBase58 == coordinationAddr))
  val witnessOutput = initOPReturn.map(_._1.getOutputs.asScala.find(o => o.getScriptPubKey.isSentToAddress && o.getAddressFromP2PKHScript(params).toBase58 == witnessAddr))

  val txsByOutput = txs.flatMap(_.getOutputs.asScala).map{o=> o->
    (if(o.getAddressFromP2PKHScript(params) != null) Some(o.getAddressFromP2PKHScript(params))
    else if (o.getAddressFromP2SH(params) != null ) Some(o.getAddressFromP2SH(params))
    else None)
  }

  val firstCoordTx = {
    val coordinationOutputs = txsByOutput.filter(tx=>tx._2.isDefined && tx._2.get.toBase58 == coordinationAddr)
    if(coordinationOutputs.isEmpty){
      None
    }
    else {
      Some( coordinationOutputs.minBy(_._1.getParentTransaction.getUpdateTime))
    }
  }

  //is the first transaction to the coordination an output of the init transaction?
  val firstCoordTxIsInit = firstCoordTx.isDefined && initOPReturn.isDefined && initOPReturn.get._1.getHash.compareTo(firstCoordTx.get._1.getParentTransaction.getHash) == 0

  //  if (initOPReturn.isDefined) println("found!")
  //  if (initOPReturn.isDefined &&
  //    witnessOutput.isDefined &&
  //    coordinationOutput.isDefined) {
  //    println("both required output addrs were found.")
  //  }
  //

  def isValid = {
    firstCoordTxIsInit && witnessOutput.isDefined
  }


  def voters = {
    val registrationTxns = op_returns.filter(op=>op._2 == "witness" && utils.Utils.passedThrough(params, op._1, witnessAddr))

    //TODO: change addr will look like it's registered...
    val o = registrationTxns.flatMap(tx=>tx._1.getOutputs.asScala.flatMap(o=> Option(o.getAddressFromP2PKHScript(params))))
    o
  }

  def votes = {
    val votersNow = voters
    val voteTxns0 = op_returns.filter(op=>op._2 == "voteFor")

    val voteTxns = voteTxns0.map(op=>op._1->votersNow.filter(a=>utils.Utils.passedThrough(params, op._1, a.toBase58)))
      .filter(a=>a._2.length == 1)
    //what happens if voters join together?
    //nothing? only take votes with exactly one voter passthrough?

    //only the last vote per voter counts.
    val finalVotes = voteTxns.groupBy(t=>t._2.head).mapValues(a=>a.maxBy(b=>b._1.getUpdateTime))

    //only the nonchange outputs (to addresses besides the origination address) are counted.
    val o = finalVotes.map{v=>v._2._1.getOutputs.asScala
      .filter(outp=>outp.getAddressFromP2PKHScript(params) != null && outp.getAddressFromP2PKHScript(params).toBase58 != v._1.toBase58)
      .maxBy(_.getValue).getAddressFromP2PKHScript(params).toBase58}
      .toSeq.groupBy(a=>a).mapValues(_.length)

    o
  }
}


object Audit extends App{
  val coordinationAddr = args.find(_.startsWith("coordinationAddr=")).get.split("=")(1)
  val witnessAddr = args.find(_.startsWith("witnessAddr=")).get.split("=")(1)
  val es = new ElectionState(coordinationAddr, witnessAddr,"1234567")

  println("registered vote addrs: \n" + es.voters.mkString("\n"))

  println("isFinished: " + "false")

  println("leading votes:\n" + es.votes.toSeq.sortBy(_._2).takeRight(10).mkString("\n"))
}