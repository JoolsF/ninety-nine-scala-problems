package ninetynineproblems

import org.scalatest.{FlatSpec, Matchers}


class OneToTenTest extends FlatSpec with Matchers {

  import OneToTen._

  "Problem 1 solution " should "find the last element of a list" in {
    last(List(1, 1, 2, 3, 5, 8)) shouldBe 8
  }

  it should "throw an error for an empty list" in {
    a[Error] should be thrownBy {
      last(List())
    }
  }


  "Problem 2 solution " should "find the penultimate element of a list" in {
    penultimate(List(1, 1, 2, 3, 5, 8)) shouldBe 5
  }

  it should "throw an error for an empty list" in {
    a[Error] should be thrownBy {
      penultimate(List())
    }
  }

  it should "throw an error for a list with one element" in {
    a[Error] should be thrownBy {
      penultimate(List(1))
    }
  }

  "Problem 3 solution " should "find the nth element of a list" in {
    nth(2, List(1, 1, 2, 3, 5, 8)) shouldBe 2
  }

  it should "throw an error for an empty list" in {
    a[Error] should be thrownBy {
      nth(3, List())
    }
  }

  it should "throw an error if n + 1 is greater than the length of the list" in {
    a[Error] should be thrownBy {
      nth(1, List(3))
    }
  }

  "Problem 4 solution " should "find the length of a list" in {
    seqLength(List(1, 1, 2, 3, 5, 8)) shouldBe 6
  }

  it should "return 0 for an empty list " in {
    seqLength(List()) shouldBe 0
  }

  "Problem 5 solution " should "reverse a list" in {
    reverse(List(1, 1, 2, 3, 5, 8)) shouldBe List(8, 5, 3, 2, 1, 1)
  }

  it should "reverse an empty list" in {
    reverse(List()) shouldBe List()
  }

  it should "reverse a list with one element" in {
    reverse(List(1)) shouldBe List(1)
  }

  it should "reverse a list with two elements" in {
    reverse(List(1, 2)) shouldBe List(2, 1)
  }

  "Problem 6 solution" should "correctly check whether a list is a palindrome" in {
    isPalindrome(List(1, 2, 3, 2, 1)) shouldBe true
    isPalindrome(List(1, 2, 3, 2, 2)) shouldBe false
  }

  it should "check whether an empty list is a palindrome" in {
    isPalindrome(Nil) shouldBe true
  }

  it should "check whether a list of element is a palindrome" in {
    isPalindrome(List(1)) shouldBe true
  }

  "Problem 7 solution" should "flatten a nested list" in {
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) shouldBe List(1, 1, 2, 3, 5, 8)
  }

  "Problem 8 solution" should "eliminate consecutive duplicates of list elements." in {
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List('a, 'b, 'c, 'a, 'd, 'e)
  }

  it should "eliminate duplicates in a 2 element list where all elements are the same" in {
    compress(List(1, 1)) shouldBe List(1)
  }

  it should "eliminate duplicates in a 3 element list where all elements are the same" in {
    compress(List(1, 1, 1)) shouldBe List(1)
  }


}
