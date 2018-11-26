structure NoGen =
struct

  type character =
    {name:string, role:string, pronoun:string, age:int}

  fun timeNowSeconds () =
    LargeInt.toInt
    (LargeInt.mod
    (Time.toSeconds (Time.now ()), 29))

  val r = Random.rand (0,timeNowSeconds())

  fun member x l = List.exists (fn y => y=x) l

  fun pastCharacter someone (cast : character list) =
    List.exists (fn {name,...} => name=someone) cast

  fun randomMember l =
  let
    val idx = Random.randRange (0, List.length l-1) r
  in
    List.nth (l,idx)
  end

  (* Given a list l, pick a random member m such that
  * m::l' is a permutation of l, return (m, l')
  *)
  fun randomPivot (l : 'a list) : ('a * 'a list) =
  let
    val idx = Random.randRange (0, List.length l -1) r
    val (pre, x::post) = List.splitAt (l, idx)
  in
    (x, pre@post)
  end

  val cards = ["fool", "magician", "high-priestess", "empress", "emperor",
  "hierophant", "lovers", "chariot", "strength", "hermit", "wheel-of-fortune",
  "justice", "hanged-man", "death", "temperance", "devil", "tower", "star",
  "moon", "sun", "judgment", "world"]

  val names_pronouns =
    [("Anna", "she"),
     ("Leon", "he"),
     ("Edward", "he"),
     ("Raul", "he"),
     ("Maria", "she"),
     ("Felix", "he"),
     ("Zelda", "she"),
     ("Roberta", "she"),
     ("Thor", "he"),
     ("Loki", "he"),
     ("Hela", "she"),
     ("Eris", "she"),
     ("Nnedi", "she"),
     ("Fiona", "she"),
     ("Aragorn", "he"),
     ("Piotr", "he"),
     ("Capheus", "he"),
     ("Wolfgang", "he"),
     ("Alexis", "they"),
     ("Zee", "they"),
     ("Beauregard", "they"),
     ("Silas", "they"),
     ("Mar", "they")]

  val (min_age, max_age) = (18, 50)
  val death_age = 100
  
  val fullCast : character list ref = ref []

  fun addSuffix (n,p) =
    if pastCharacter n (!fullCast) then
      if String.isSuffix "I" n then n^"I" else n^" II"
    else n

  fun genCharacter () =
  let
    (* val ((name, pronoun), rest) = randomPivot (!names_pronouns)
    val () = names_pronouns := rest@[(name, pronoun)] *)
    val (name, pronoun) = randomMember names_pronouns
    val name = addSuffix (name, pronoun)
    val age = Random.randRange (min_age, max_age) r
    val role = randomMember cards
  in
    {name=name, role=role, pronoun=pronoun, age=age}
  end

  fun makeN f n =
    case n of
         0 => []
       | n => (f ())::(makeN f (n-1))

  val characters = makeN genCharacter 4

  val () = fullCast := characters
  val deaths = ref 0
  
  (*
  val characters = 
    [{name="Anna", role=randomMember cards, pronoun="she", age=18},
     {name="Roberta", role=randomMember cards, pronoun="she", age=54},
     {name="Leon", role=randomMember cards, pronoun="he", age=28}]
  *)

  fun getCharacterMeanings ch =
  let
    val file = TextIO.openIn (ch^".txt")
    fun addlines lines = 
      case TextIO.inputLine file of 
            SOME line => addlines (line::lines)
          | NONE => rev lines
    val meanings = addlines []
  in
    meanings
  end

  val meaningMaps : (string * string list) list =
    map (fn card => (card, getCharacterMeanings card)) cards

  fun lookup x dict =
    case dict of [] => NONE
       | ((y,ms)::dict) => if x = y then SOME ms else lookup x dict

  fun notWord c = Char.isSpace c orelse Char.isPunct c

  fun secondToThirdPerson (third : string) (second : string) : string =
    case second of
         "you" => third
       | "your" => (case third of "she" => "her" | "he" => "his" | _ => "their")
       | "yourself" => (case third of "she" => "herself" | "he" => "himself" | _
       => "themselves")
       | "are" => "is"
       | other => other

  fun replaceSecondWithThird s pronoun =
  let
    val words = String.tokens notWord s
    val words' = map (secondToThirdPerson pronoun) words 
  in
    String.concatWith " " words'
  end

  fun introduceCharacter {name,role,pronoun,age} =
    name ^ " (" ^ role ^ "), " ^ (Int.toString age) 
    ^ " years old, enters the story.\n"

  fun getOlder {name,role,pronoun,age} chapter =
    if age >= death_age then (* char dies *)
      let
        val deathLine = name ^ " dies.\n\n"
        val chapterLine = "Chapter " ^ (Int.toString (chapter+1) ^ "\n\n")
        val newChar = genCharacter ()
        val () = fullCast := (newChar::(!fullCast))
        val () = deaths := !deaths + 1
        val birthLine = introduceCharacter newChar
      in
        (deathLine^chapterLine^birthLine, newChar, chapter+1)
      end
    else
      ("", {name=name,role=role,pronoun=pronoun,age=age+1}, chapter) 


  fun genLine characters chapter =
  let
    val (star, otherChars) = randomPivot characters
    val {name, role, pronoun, age} = star
    val action = randomMember (valOf (lookup role meaningMaps))
    val modAction = replaceSecondWithThird action pronoun
    val line = name ^ ", " ^ Int.toString age ^ ", is " ^ modAction ^ "\n"
    val words = String.tokens notWord line
    val (moreLines, newChar, newChapter) = getOlder star chapter
  in
    (line^moreLines, List.length words, newChar::otherChars, newChapter)
  end

  fun gen' words target story chars (chapter:int) =
    if words > target then {story = story, cast = chars}
    else
      let
        val (more, length, chars', chapter') = genLine chars chapter 
      in
        gen' (words+length) target (story^more) chars' chapter'
      end

  fun gen target = 
    let
      val chapterHeader = "Chapter 1\n\n"
      val intro = String.concat (map introduceCharacter characters)
      val {cast, story} = gen' 0 target "" characters 1
    in
      {cast=cast, story = chapterHeader ^ intro ^ story}
    end

end
