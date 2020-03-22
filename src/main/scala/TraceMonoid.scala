import scala.io.Source

object TraceMonoid {
  val acceptedVariables = "[a-z]"

  def parse(transactionStr: String, alphabet: List[Char]): (Char, Char, List[Char]) = {
    val leftSide: String = transactionStr.split('=')(0)
    val rightSide: String = transactionStr.split('=')(1)

    //filtrowanie z np. znakow dodawania:
    val leftFiltered: List[Char] = acceptedVariables.r.findAllMatchIn(leftSide).toList.map(a => a.toString().head)
    val rightFiltered: List[Char] = acceptedVariables.r.findAllMatchIn(rightSide).toList.map(a => a.toString().head)

    (alphabet.head, leftFiltered.head, rightFiltered)
  }

  def isDependent(pair: (Char, Char), transactions: List[(Char, Char, List[Char])]): Boolean = {
    val (letter1, letter2) = pair
    //sprawdzam zaleznosc samej ze soba
    if (letter1 == letter2)
      return true
    //wybieram transakcje dla danych liter
    val transaction1: (Char, Char, List[Char]) = transactions.filter(x => (x._1 == letter1)).head
    val transaction2: (Char, Char, List[Char]) = transactions.filter(x => (x._1 == letter2)).head

    //bezposrednio sprawdzam zaleznosc poszczegolnych transakcji
    (transaction1._2 == transaction2._2)||(transaction1._3.contains(transaction2._2))||(transaction2._3.contains(transaction1._2))
  }

  def enumerateOperations(word: List[Char]): List[String] = {
    var operations: List[String] = Nil

    for (i <- word.indices) {
      val operation = word(i)
      var counter = 1
      for (j <- 0 until i)
        if (word(j) == operation)
          counter += 1
      operations = operations ++ List(operation.toString ++ counter.toString)
    }

    operations
  }

  def calculateFoataClass(operations: List[String], relationOfDependency: List[(Char, Char)]): List[String] = {
    if (operations == Nil)
      return Nil

    val operation = operations.last
    val dependentOperations = operations.filter(a => relationOfDependency.contains((operation.head, a.head)))
    //jesli nie znajde operacji od ktorych zalezne (zawsze bedzie zalezne ze soba samym)
    // jest operation to dodaje je do klasy foaty
    //szukam dalej jej czlonkow wsrod poprzedzajacych operacji
    if (dependentOperations == List(operation))
      return calculateFoataClass(operations.dropRight(1), relationOfDependency) ++ List(operation)

    //jesli znajde - nie dodaje do klasy foaty
    calculateFoataClass(operations.dropRight(1), relationOfDependency)
  }

  def calculateFoataNormalForm(operations: List[String], relationOfDependency: List[(Char, Char)]): List[List[String]] = {
    if (operations == Nil)
      return Nil

    //wyliczam klase foaty, dodaję ją na poczatek listy
    // i powtarzam operacje dla tych relacji ktore nie zawieraja dodanej przed chwila operacji
    val firstFoataClass = calculateFoataClass(operations, relationOfDependency)
    firstFoataClass :: calculateFoataNormalForm(operations.filter(a => !firstFoataClass.contains(a)), relationOfDependency)
  }

  def generateGraph(FNF: List[List[String]], dependencyRelation: List[(Char, Char)]): List[(String, String)] = {
    var edges: List[(String, String)] = Nil
    var foundBelow = false
    for (i<-1 until FNF.size){
      for (g<- FNF(i-1).indices) {
        foundBelow = false
        for (j<- FNF(i).indices) {
          if (dependencyRelation.contains((FNF(i-1)(g).head, FNF(i)(j).head))) {
            edges = (FNF(i-1)(g).toString, FNF(i)(j).toString) :: edges
            foundBelow = true
          } else {
            if (j==FNF(i).size-1 && !foundBelow) {
              var foundEdgeOnLowerLvl = false
              for (k <- i + 1 until FNF.size) {
                if (!foundEdgeOnLowerLvl) {
                  for (h <- FNF(k).indices) {
                    if (dependencyRelation.contains((FNF(i - 1)(g).head, FNF(k)(h).head))) {
                      edges = (FNF(i - 1)(g).toString, FNF(k)(h).toString) :: edges
                      foundEdgeOnLowerLvl = true
                    }
                  }
                }
              }
              foundBelow = true
            }
          }
        }
      }
    }
    edges
  }

  def FNFFromGraph(vertexes: List[String], edges: List[(String, String)]): List[List[String]] = {
    if (vertexes.isEmpty)
      return Nil
    //wybieram wierzcholki koncowe grafu
    val endVertexes = edges.map(a => a._2)
    //filtruje ciag operacji w taki sposob aby nie miec w nich wierzcholkow koncowych - bedzie to klasa foaty
    val foataClass = vertexes.filter(a => !endVertexes.contains(a)).distinct;
    //przebudowuje graf pozbywając sie wierzcholkow nalezacych do klasy Foaty
    val newEdges = edges.filter(a => !foataClass.contains(a._1))
    //szukam kolejnych klas
    foataClass :: FNFFromGraph(endVertexes, newEdges)
  }

  def printRelation(relation: List[(Char, Char)], relationType :Char): Unit={
    println(relationType + " = {")
    for ((a, b) <- relation) {
      println("(" + a + ", " + b + "),")
    }
    println("}")
  }

  def printFNF(word: List[Char], FNF: List[List[String]]): Unit={
    print("FNF([" + word + "]) = ")
    for (foataClass <- FNF) {
      print("( ")
      for (operation <- foataClass) {
        print(operation + " ")
      }
      print(")")
    }
    println()
  }

  def printGraph(edges: List[(String, String)], operations: List[String]): Unit={
    println("digraph g{")
    for (edge <- edges) {
      val (left, right) = edge
      println(left + " -> " + right)
    }
    for (operation <- operations) {
      println(operation + " [label = " + operation.head + "]")
    }
    println("}")
  }

  def main(args: Array[String]): Unit = {
    val bufferedSource = Source.fromFile("test_data4")
    val lines = bufferedSource.getLines.toBuffer
    bufferedSource.close

    val alphabetSize = lines.head.toInt
    lines.remove(0)
    var alphabet: List[Char] = Nil
    var transactions: List[(Char, Char, List[Char])] = Nil

    //parsowanie inputu
    for (_ <- 1 to alphabetSize) {
      alphabet = lines.head.charAt(0) :: alphabet
      lines.remove(0)
      val transactionString: String = lines.head
      lines.remove(0)
      transactions = parse(transactionString, alphabet) :: transactions
    }

    //wyznaczanie relacji
    val allPairsOfLetters = alphabet.flatMap(a => alphabet.map(b => (a, b)))
    val dependencyRelation = allPairsOfLetters.filter(isDependent(_, transactions))
    val independencyRelation = allPairsOfLetters.filterNot(dependencyRelation.contains(_))

    printRelation(dependencyRelation, 'D')
    printRelation(independencyRelation, 'I')

    val word: List[Char] = lines.head.toList
    //dla kazdej operacji w slowie nadaję jej kolejny numer
    val operations = enumerateOperations(word)

    //wyznaczanie postaci normalnej foaty na podstawie relacji zaleznosci
    val FNF = calculateFoataNormalForm(operations, dependencyRelation)
    printFNF(word, FNF)

    //wygenerowanie grafu Diekerta
    val edges = generateGraph(FNF, dependencyRelation)
    printGraph(edges, operations);

    //wygenerowanie FNF na podstawie grafu Diekerta
    val FNF2 = FNFFromGraph(operations, edges)
    printFNF(word, FNF2)
  }
}