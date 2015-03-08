package com.mike.scalalearning.web1.p10

import sun.awt.TextureSizeConstraining

/**
 * Created by chufang on 15-2-27.
 */
case class Email(sender: String, receiver: String, subject:String, text:String)
object Wrapper extends App{
  type EmailFilter = Email => Boolean
  def newEmailsForUser(mails: Seq[Email], emailFilter: EmailFilter) = mails.filter(emailFilter)

  val sentByOneOf: Set[String] => EmailFilter = senders => email => senders.contains(email.sender)
  /*val notSentByAnyOf: Set[String] => EmailFilter = senders => email => !senders.contains(email.sender)*/
  /*val minimumSize: Int => EmailFilter = n => email => email.text.length > n
  val maximumSize: Int => EmailFilter = n => email => email.text.length < n*/

  type SizeChecker = Int => Boolean
  val sizeConstraint:SizeChecker => EmailFilter = f => email => f(email.text.length)

  val minimumSize:Int => EmailFilter = n => sizeConstraint(_ > n)
  val maximumSize:Int => EmailFilter = n => sizeConstraint(_ < n)

  def complement[A](predicate: A=>Boolean): A=>Boolean = x => !predicate(x)

  val notSentByAnyOf = sentByOneOf.andThen(complement(_))

  def any[A](predicates: (A => Boolean)*): A=>Boolean = a => predicates.exists(pred => pred(a))
  def none[A](predicates: (A=>Boolean)*): A=>Boolean = complement(any(predicates: _*))
  val mail = Email("mike@google.com", "George@paypal.com", "Greeting", "Hello you")
  val filter = sentByOneOf(Set("ge@google.com"))
  println(newEmailsForUser(mail::Nil, filter))
}

