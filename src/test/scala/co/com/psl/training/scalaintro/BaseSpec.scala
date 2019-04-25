package co.com.psl.training.scalaintro

// Import the ScalaTest style traits to be used uniformly in this project.
import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TypeCheckedTripleEquals

/** Base class that defines the style for all tests suites. */
abstract class BaseSpec extends FlatSpec with Matchers with TypeCheckedTripleEquals
