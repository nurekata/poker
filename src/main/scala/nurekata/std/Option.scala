package nurekata.std

enum Option[+A]:
   case None
   case Some(a: A)

   def isEmpty: Boolean = this == None

   def get: A =
      this match
         case Some(a) => a
         case None    => throw new NoSuchElementException("None.get")

   def map[B](f: A => B): Option[B] =
      this match
         case None    => None
         case Some(a) => Some(f(a))

   def exists(p: A => Boolean): Boolean =
      this match
         case None    => false
         case Some(a) => p(a)

   def orElse[B >: A](alt: => Option[B]): Option[B] =
      this match
         case None    => alt
         case Some(a) => Some(a)

   def getOrElse[B >: A](default: => B): B =
      this match
         case None    => default
         case Some(a) => a