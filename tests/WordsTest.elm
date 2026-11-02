module WordsTest exposing (suite)

{-| Cenários do spec `word-list` que dá para conferir nos dados.

"Palavras conhecidas pelo público geral" é um critério editorial: aqui só se
conferem os exemplos do spec. "Tela de preparação sem seleção de lista" está
em `SetupTest`, que não tem esse controle.

-}

import Array
import Expect
import Main
import Set
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text)
import Words


words : List String
words =
    Array.toList Words.words


wordSet : Set.Set String
wordSet =
    Set.fromList words


suite : Test
suite =
    describe "word-list"
        [ describe "Lista única de palavras"
            [ test "Tela de preparação sem seleção de lista" <|
                \_ ->
                    Main.view (Tuple.first Main.init)
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.hasNot [ text "Palavras verificadas" ]
                            , Query.hasNot [ text "Todas as palavras" ]
                            , Query.has [ text "Tempo da rodada" ]
                            , Query.has [ text "Jogadores" ]
                            ]
            ]
        , describe "Sem duplicatas"
            [ test "cada palavra aparece uma vez" <|
                \_ -> Set.size wordSet |> Expect.equal (List.length words)
            , test "Palavra repetida na origem" <|
                \_ ->
                    List.filter (\word -> word == "Cacto" || word == "Ducha") words
                        |> Expect.equal [ "Cacto", "Ducha" ]
            ]
        , describe "Palavras bem formadas"
            [ test "começam com maiúscula e só têm letras e hífen interno" <|
                \_ ->
                    List.filter (not << wellFormed) words
                        |> Expect.equal []
            , test "Entrada corrompida é excluída" <|
                \_ ->
                    [ "Phil ips", "Charle sto.", "Araini ng", "Houveu" ]
                        |> List.filter (\word -> Set.member word wordSet)
                        |> Expect.equal []
            , test "Estrangeirismo corrente é aceito" <|
                \_ ->
                    [ "Hacker", "Handebol" ]
                        |> List.filter (\word -> not (Set.member word wordSet))
                        |> Expect.equal []
            ]
        , describe "Palavras conhecidas pelo público geral"
            [ test "Palavra comum é mantida" <|
                \_ ->
                    [ "Hospital", "Capacete", "Zumbir" ]
                        |> List.filter (\word -> not (Set.member word wordSet))
                        |> Expect.equal []
            , test "Palavra obscura é excluída" <|
                \_ ->
                    [ "Androceu", "Háfnio", "Haplologia", "Hamadríade" ]
                        |> List.filter (\word -> Set.member word wordSet)
                        |> Expect.equal []
            ]
        , describe "Cobertura de todo o alfabeto"
            [ test "Letras antes não verificadas aparecem" <|
                \_ ->
                    let
                        initials =
                            Set.fromList (List.filterMap (String.left 1 >> Just) words)
                    in
                    [ "I", "M", "P", "S", "Z" ]
                        |> List.filter (\letter -> not (Set.member letter initials))
                        |> Expect.equal []
            ]
        ]


wellFormed : String -> Bool
wellFormed word =
    case String.uncons word of
        Just ( first, rest ) ->
            isUpper first
                && String.all (\char -> Char.isAlpha char || isAccented char || char == '-') rest
                && not (String.endsWith "-" word)
                && not (String.contains "--" word)

        Nothing ->
            False


{-| `Char.isUpper` só reconhece ASCII; isto vale também para "Á", "Ô"…
-}
isUpper : Char -> Bool
isUpper char =
    let
        letter =
            String.fromChar char
    in
    String.toUpper letter == letter && String.toLower letter /= letter


{-| `Char.isAlpha` só reconhece ASCII.
-}
isAccented : Char -> Bool
isAccented char =
    String.contains (String.fromChar char) "áàâãéêíóôõúüçÁÀÂÃÉÊÍÓÔÕÚÜÇ"
