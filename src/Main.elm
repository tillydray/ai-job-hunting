module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (for, id)
import Html.Events exposing (onBlur, onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Encode as E


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Html Msg
view model =
    Html.main_ []
        [ Html.form [ onSubmit Submit ]
            (resumeInput model.rawResume model.resumeError
                ++ jobDescriptionInput model.rawJobDescription model.jobDescriptionError
                ++ writingSampleInput model.rawWritingSample
                ++ submitButton model.submitError
                ++ responseDisplay model.responseSuccess
                ++ responseErrorDisplay model.responseError
            )
        ]


resumeInput : String -> Maybe Error -> List (Html Msg)
resumeInput rawResume resumeError =
    [ Html.label [ for "resume-input" ] [ Html.text "Resume" ]
    , Html.textarea [ id "resume-input", onInput UpdateRawResume, onBlur ResumeBlur ]
        [ Html.text rawResume ]
    ]
        ++ renderError resumeError


jobDescriptionInput : String -> Maybe Error -> List (Html Msg)
jobDescriptionInput rawJobDescription jobDescriptionError =
    [ Html.label [ for "job-description-input" ] [ Html.text "Job Description" ]
    , Html.textarea [ id "job-description-input", onInput UpdateRawJobDescription, onBlur JobDescriptionBlur ]
        [ Html.text rawJobDescription ]
    ]
        ++ renderError jobDescriptionError


writingSampleInput : String -> List (Html Msg)
writingSampleInput rawWritingSample =
    [ Html.label [ for "writing-sample-input" ] [ Html.text "Writing Sample" ]
    , Html.textarea [ id "writing-sample-input", onInput UpdateRawWritingSample ]
        [ Html.text rawWritingSample ]
    ]


responseDisplay : String -> List (Html Msg)
responseDisplay response =
    [ Html.label [ for "response-display" ] [ Html.text "Cover Letter" ]
    , Html.textarea [ id "response-display" ]
        [ Html.text response ]
    ]


responseErrorDisplay : Maybe Http.Error -> List (Html Msg)
responseErrorDisplay maybeError =
    case maybeError of
        Nothing ->
            []

        Just error ->
            case error of
                Http.BadBody e ->
                    [ Html.label [ for "response-display" ] [ Html.text "Cover Letter" ]
                    , Html.textarea [ id "response-display" ]
                        [ Html.text e ]
                    ]

                _ ->
                    [ Html.label [ for "response-display" ] [ Html.text "Cover Letter" ]
                    , Html.textarea [ id "response-display" ]
                        [ Html.text "idk" ]
                    ]


submitButton : Maybe Error -> List (Html Msg)
submitButton submitError =
    Html.button [] [ Html.text "Submit" ] :: renderError submitError


renderError : Maybe Error -> List (Html Msg)
renderError error =
    case error of
        Just err ->
            [ Html.p [] [ Html.text <| errorToString err ] ]

        Nothing ->
            []


errorToString : Error -> String
errorToString error =
    case error of
        ResumeEmpty ->
            "Must enter a resume"

        JobDescriptionEmpty ->
            "Must enter a job description"

        SubmitError ->
            "Fix all errors then try again"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResumeBlur ->
            let
                result =
                    mkResume model.rawResume
            in
            case result of
                Ok resume ->
                    ( { model | resume = Just resume }, Cmd.none )

                Err err ->
                    ( { model | resumeError = Just err }, Cmd.none )

        UpdateRawResume string ->
            ( { model | rawResume = string, resumeError = Nothing, submitError = Nothing }, Cmd.none )

        JobDescriptionBlur ->
            let
                result =
                    mkJobDescription model.rawJobDescription
            in
            case result of
                Ok jobDescription ->
                    ( { model | jobDescription = Just jobDescription }, Cmd.none )

                Err err ->
                    ( { model | jobDescriptionError = Just err }, Cmd.none )

        UpdateRawJobDescription string ->
            ( { model | rawJobDescription = string, submitError = Nothing }, Cmd.none )

        UpdateRawWritingSample string ->
            ( { model | rawWritingSample = string }, Cmd.none )

        Submit ->
            let
                isFormValid =
                    isJust model.resume && isJust model.jobDescription
            in
            if isFormValid then
                ( model, postDocs model )

            else
                ( { model | submitError = Just SubmitError }, Cmd.none )

        GotCoverLetter result ->
            case result of
                Err err ->
                    ( { model | responseError = Just err }, Cmd.none )

                Ok json ->
                    case D.decodeString decodeJsonData (Debug.log "json" json) of
                        Ok content ->
                            ( { model | responseSuccess = content }, Cmd.none )

                        Err err ->
                            ( { model | decodeError = Just (Debug.log "DEBUG" err) }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


postDocs : Model -> Cmd Msg
postDocs model =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" "Bearer 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" ]
        , url = "https://api.openai.com/v1/chat/completions"
        , body = mkPostBody model
        , expect = Http.expectJson GotCoverLetter decodeJsonData
        , timeout = Nothing
        , tracker = Nothing
        }


mkPostBody : Model -> Http.Body
mkPostBody model =
    let
        systemObject =
            [ ( "role", E.string "system" ), ( "content", E.string "You are a career coach for software engineers that specializes in writing cover letters." ) ]

        resumeObject resume =
            [ ( "role", E.string "user" ), ( "content", E.string <| "Here is my resume. I want you to learn it.  " ++ unResume resume ) ]

        jobDescriptionObject jobDescription =
            [ ( "role", E.string "user" ), ( "content", E.string <| "Here is a job description. I want you to learn it.  " ++ unJobDescription jobDescription ) ]

        writingSampleObject writingSample =
            if String.isEmpty writingSample then
                []

            else
                [ ( "role", E.string "user" )
                , ( "content"
                  , E.string <| "Here is a cover letter that I think is good. I want you to learn its content and its writing style so you can recreate the writing style. \n" ++ writingSample
                  )
                ]
    in
    case ( model.resume, model.jobDescription, model.rawWritingSample ) of
        ( Just resume, Just jobDescription, writingSample ) ->
            Http.jsonBody <|
                E.object
                    [ ( "model", E.string "gpt-3.5-turbo" )
                    , ( "messages"
                      , E.list E.object [ systemObject, resumeObject resume, jobDescriptionObject jobDescription, writingSampleObject writingSample ]
                      )
                    ]

        _ ->
            -- TODO
            Http.emptyBody


unJobDescription : JobDescription -> String
unJobDescription (JobDescription j) =
    j


type alias Model =
    { resume : Maybe Resume
    , rawResume : String
    , resumeError : Maybe Error
    , jobDescription : Maybe JobDescription
    , rawJobDescription : String
    , jobDescriptionError : Maybe Error
    , rawWritingSample : String
    , submitError : Maybe Error
    , responseSuccess : String
    , responseError : Maybe Http.Error
    , decodeError : Maybe D.Error
    }


initModel : Model
initModel =
    { resume = Nothing
    , rawResume = ""
    , resumeError = Nothing
    , jobDescription = Nothing
    , rawJobDescription = ""
    , jobDescriptionError = Nothing
    , rawWritingSample = ""
    , submitError = Nothing
    , responseSuccess = ""
    , responseError = Nothing
    , decodeError = Nothing
    }



-- decodeJsonList =
--     D.field "choices" D.list
--         |> D.andThen
--             (\list ->
--                 case list of
--                     [] ->
--                         D.fail "Empty list"
--                     x :: xs ->
--                         decodeJsonData x
--             )


decodeJsonData : D.Decoder String
decodeJsonData =
    D.field "choices"
        (D.index 0
            (D.field "message"
                (D.field "content" D.string)
            )
        )


type Error
    = ResumeEmpty
    | JobDescriptionEmpty
    | SubmitError


type Resume
    = Resume String


unResume : Resume -> String
unResume (Resume r) =
    r


mkResume : String -> Result Error Resume
mkResume string =
    if String.isEmpty string then
        Err ResumeEmpty

    else
        Ok <| Resume string


type JobDescription
    = JobDescription String


mkJobDescription : String -> Result Error JobDescription
mkJobDescription string =
    if String.isEmpty string then
        Err JobDescriptionEmpty

    else
        Ok <| JobDescription string


type WritingSample
    = WritingSample String


type Msg
    = UpdateRawResume String
    | ResumeBlur
    | UpdateRawJobDescription String
    | JobDescriptionBlur
    | UpdateRawWritingSample String
    | Submit
    | GotCoverLetter (Result Http.Error String)
    | NoOp



------------------------ Helpers ------------------------


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        _ ->
            False
