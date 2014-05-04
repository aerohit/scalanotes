package neophytes

case class Email(
  subject: String,
  text: String,
  sender: String,
  recepient: String)

object EmailFiltering {
  type EmailFilter = Email => Boolean
  def newMailsForUser(mails: Seq[Email], f: EmailFilter): Seq[Email] = mails.filter(f)
}

object HigherOrder extends App {
   
}
