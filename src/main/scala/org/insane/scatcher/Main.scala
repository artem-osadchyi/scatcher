package org.insane.scatcher

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import org.jsoup.Jsoup
import org.jsoup.nodes.Document

import scala.collection.JavaConverters._
import scala.io.Source

object Main extends App {

  //  val documentSource = new LocalFileDocumentSource("src/test/resources/monthes_page_available.html")
  val documentSource = new WebDocumentSource
  val appointmentsService = new AppointmentService(documentSource.get, "http://service.berlin.de/dienstleistung/120686/terminall/")

  appointmentsService.available.foreach(println)

}

class AppointmentService(client: (String) => Document, url: String) {

  def available: Seq[Appointment] = {
    for {
      monthTable <- client(url).select("div.calendar-month-table").asScala
      monthYear = monthTable.select("th.month").text.trim
      availableDayElement <- monthTable.select("td.buchbar").asScala
      dayOfMonth = availableDayElement.text.trim
      dateUrl = availableDayElement.select("a.tagesauswahl").attr("href")
      appointmentsDocument = client(dateUrl)
      timeElement <- appointmentsDocument.select("div.timetable > table > tbody > tr").asScala
      time = timeElement.select("th.buchbar").text.trim
      buergeramt = timeElement.select("td.frei").text.trim
      bookUrl = timeElement.select("td.frei > a").attr("href")
    } yield new Appointment(parseTime(monthYear, dayOfMonth, time), buergeramt, bookUrl)
  }

  private def parseTime(yearMonth: String, dayOfMonth: String, time: String): LocalDateTime = {
    val dateString = f"${dayOfMonth.toInt}%02d $yearMonth $time"
    val formatter = DateTimeFormatter.ofPattern("dd MMMM yyyy HH:mm") //, Locale.GERMANY)

    LocalDateTime.parse(dateString, formatter)
  }

}

trait DocumentSource {

  def get(): Document

  def get(url: String): Document

}

class WebDocumentSource extends DocumentSource {
  val Url = "http://service.berlin.de/dienstleistung/120686/terminall/"
  val Timeout = 10000

  def get(): Document = get(Url)

  def get(url: String): Document = {
    print(s"Connecting to $url...")
    val result = Jsoup.connect(url).timeout(Timeout).get()
    println("Success")
    println(result)
    result
  }

}

class LocalFileDocumentSource(path: String) extends DocumentSource {

  def get(): Document = get(path)

  def get(url: String): Document = Jsoup.parse(Source.fromFile(url).getLines().mkString("\n"))

}

class Appointment(val time: LocalDateTime, val buergeramt: String, private val bookUrl: String) {

  override def toString = s"$time at $buergeramt ($bookUrl)"

}
