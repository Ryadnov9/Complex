import scala.language.implicitConversions
import scala.math._
// определение класса образца => адекватная реализация equals (сравниваются по структуре, а не по ссылкам), hashCode, сопутствующий объект с методом apply factory,  параметры класса неявно определены как val
case class Complex(re: Double, im: Double) extends Ordered[Complex] //  унаследование черты Ordered, которая обеспечивает реализацию 4 операторов сравнения(>, >=, <, <=), которые вызывают абстрактный метод compare
{
  private val modulus = sqrt(pow(re, 2) + pow(im, 2))
  def this(re: Double) = this(re, 0); // перегрузка конструктора для комплексных чисел с нулевой мнимой частью

  def unary_+ : Complex = this
  def unary_- = new Complex(-re, -im); // нахождение комплексного числа обратного к данному
  def unary_~ = new Complex(re, -im); // нахождение комлексно-сопряженного числа данному
  def unary_! : Double = modulus; // нахождение модуля комплексного числа или радиуса
  def compare(that: Complex): Int = !this compare !that; // предоставление конкретной реализации метода compare,
  def +(c: Complex) = new Complex(re + c.re, im + c.im); // реализация метода оператора +
  def -(c: Complex): Complex = this + -c; // реализация метода оператора -
  def *(c: Complex) = new Complex(re * c.re - im * c.im, im * c.re + c.im); // реализация метода оператора *
  def /(c: Complex): Complex = { // реализация метода оператора /
    require(c.re != 0 || c.im != 0, "Мнимая и действительная части делителя не  должны равняться нулю!")
    val d = pow(c.re, 2) + pow(c.im, 2)
    new Complex((re * c.re + im * c.im) / d, (im * c.re - re * c.im) / d)
  }
  // переопределение метода
  override def toString: String =
    this match { // сопоставление с образцом
      case Complex.i => "i"; // шаблон представляющий константу i
      case Complex(re, 0) => re.toString; // шаблон представляющий действительное число
      case Complex(0, im) => im.toString + "*i"; // шаблон представляющий только мнимое число
      case _ => asString; // все остальное
    }
  private def asString = re + (if (im < 0) "-" + -im else "+" + im) + "*i"

}
// объявление объекта-компаньона классу Complex - статическая реализация паттерна
object Complex {
  val i = new Complex(0, 1); // i постоянная для мнимой единицы
  def apply(re: Double) = new Complex(re); // фабричный метод, который позволяет создавать экземпляры Complex
  // неявное преобразование - паттерн адаптер
  implicit def doubleToComplex(d: Double): Complex = new Complex(d); // из Double в Complex
  implicit def floatToComplex(f: Float): Complex = new Complex(f); // из Float в Complex
  implicit def flongToComplex(l: Long): Complex = new Complex(l); // из Long в Complex
  implicit def intToComplex(i: Int): Complex = new Complex(i); // из Int в Complex
  implicit def shortToComplex(s: Short): Complex = new Complex(s); // из Short в Complex
}
import Complex._
