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



{-

   # [Jason Fry](http://JasonMFry.com)

   **Engineering Manager │ Sr. Software Engineer │ UX Engineer**

   ## Key Competencies:

   - Servant Leadership
   - Project Management
   - Team Building
   - Elm
   - Haskell
   - a11y
   - UX
   - Docker
   - IaC
   - PostgreSQL
   - Typescript

   ## Professional Expertise

   ### Engineering Manager [Vendr](https://vendr.com) 2023

   - Engineering Manager for an Agile remote team of 4 engineers. This included regular 1:1s, career growth conversations, coaching, goal setting, feedback, code reviews, and mentoring.
   - Enforced engineering best practices via coaching and code reviews.
   - Built a replacement of an internal tool using Node, TypeScript, and PostgreSQL, saving 5x the cost. New tool was 3x faster to iterate on while being more stable.
   - Created a First Team mindset among fellow Engineering Managers.
   - Created and implemented Incident Response Procedure using Sentry, AWS Cloudformation, and Slack alerts.

   ### Acting Engineering Manager, Tech Lead, + Senior Engineer [Caribou](https://caribou.com) 2021-2023

   - Acting Engineering Manager for 10 engineers on two different Agile remote teams across multiple timezones. This included regular 1:1s, career growth conversations, coaching, goal setting, feedback, code reviews, and mentoring.
   - Participated in hiring, onboarding, and talent retention.
   - De-risked the most important initiative in the business (potential gain of $10M+ per year) by combining and managing two teams while avoiding Brooks's Law. Used Haskell, Event Sourcing, and Postgres.
   - Worked closely with business stakeholders and our PMs to create requirements and decompose problems for both teams.
   - Navigated both teams through many transitions, changes in scope, expectations, etc., while still delivering the MVP on time.
   - Primary go-to for Conflict resolution between team members.
   - Built insurance.caribou.com using Elm, Haskell, Event Sourcing, and Postgres ahead of schedule with even fewer bugs than were acceptable. Maintenance requirements remain near 0.
   - Created and implemented Incident Response Procedure.
   - Improved collaboration between Design and Engineering which sped up design iterations and improved relationships and trust between teams.

   ### UX Engineer [1Password](https://1password.com) 2019-2021

   - Overhauled the A/B testing process, which led to improvements in the signup process.
   - Reduced Client Support tickets from people creating the wrong account type by 35%.
   - Reduced page-by-page drop-off rate by over 20%.
   - Helped 1Password meet WCAG 2 Level AA.
   - Participated in design critiques and ideation of potential A/B tests to run.
   - Automated the deployment process, which reduced time spent per deployment from nearly 2 hours to under 20 minutes.

   ### Software Engineer [ITPro.TV](https://itpro.tv) 2017-2019

   - Increased software team's productivity by over 30% by spearheading the adoption of "promiscuous pairing."
   - Built beautiful, maintainable, responsive web apps using Haskell, Elm, Terraform, PostgreSQL, AWS, and Docker
   - Increased software team's productivity by over 5% and reduced deployment errors by automating deployment workflow.
   - Improved developer knowledge base by implementing lunch n' learns and monthly functional programming meetups.
   - Led the software team in writing technical reports and research, increasing company publicity, and guiding future improvements.

   ### Regional Liaison SC Department of Social Services—Charleston, SC 01/2013 - 12/2015

   - Integral to the planning, execution, and control of a 50-million-dollar contract, expanding our previous model by tripling the amount of services offered.
   - Led ongoing recruitement and training.
   - Managed 10 companies representing over 150 personnel.

   ## Professional Education

   - General Assembly: Web Development
   - Bachelor of Arts (BA): Religion/Religious Studies GPA 3.6 2007 Charleston Southern University
   - License: Professional Scrum Master: Credential ID 243727


-}
{-

   Bitwarden promotes better internet security and safety with the leading open source password management solution for individuals, teams, and business organizations. Our philosophy about security is that “we are all in this together” – and so we continue to offer a full-featured free version of Bitwarden so that everyone can be protected with strong password management. At the same time, we provide enterprise-grade solutions to some of the largest companies in the world, and will continue to innovate in the identity and authentication market as the world starts to grow beyond passwords. Read more on the Bitwarden blog.

   As an Engineering Manager at Bitwarden you will lead the software development efforts that build the future of the Bitwarden product across mobile, web and server components while working with a great team and amazing open source community. We’re looking for someone who can help our team grow confidently by evaluating and identifying demands of future roadmap and product delivery needs against resources, capacity and velocity of the development team as well as facilitate career growth for direct reports to help meet that demand and foster professional development. You will own the software development process and contribute directly to the culture of quality within Bitwarden through process improvements, departmental outreach, and mentoring. This person will hire for and directly manage a cross-functional team of software engineers focused on support, web, desktop, mobile, server, and integration development. This person will help secure cross-department resources, manage trade-offs for delivery, keep stakeholders accountable, meet deadlines, communicate at the executive level on project status and risk, and measure team impact and needs.

   This is an all-remote team and US time zones are preferred. We do not offer visa sponsorship at this time.

   RESPONSIBILITIES

   Hire and develop our team of cross-functional Software Engineers

   Set and report on measurable goals and objectives for the team

   Set weekly or daily priorities for the development team and individual team members

   With feedback from the team, leadership and external stakeholders, own the development process, procedures and decision support for product development and delivery

   Evaluate, recommend, and implement development tools to help facilitate team success

   Provide a point of technical escalation and knowledge to help facilitate and drive the resolution of issues and technical or administrative hurdles the team may encounter

   Facilitate and encourage collaboration through team meetings or other communication channels as necessary, as well as conduct team-building activities

   Report on team progress, status and roadblocks to management from the development lens

   Advocate for and on behalf of the development team and the construction and delivery of Bitwarden’s product, process and organization

   Conduct one-on-one meetings with direct reports to work on issues, provide and receive feedback and discuss individual professional development goals and opportunities

   Perform annual or semi-annual performance reviews when required

   Participate in code reviews, learning and spreading technical knowledge

   Actively participate in backlog grooming, planning and delivery activities

   Become an expert and authority on the entire Bitwarden product, including: backend infrastructure, APIs, database, web app, mobile apps, browser extensions, desktop apps, and more

   WHAT YOU BRING TO BITWARDEN

   Leadership skills:

   Ability to inspire and provide vision direction

   Experience building and growing successful teams

   Management or team lead experience from a previous technical position

   Propensity towards leadership and mentoring

   Interpersonal skills:

   Ability to form good relationships with other leaders in the company

   A strong sense of empathy and the ability to advocate for others

   Collaborative and adaptable mindset

   Openness and authenticity combined with excellent communication skills

   Analytical skills:

   Strength in evaluating the success of Bitwarden in reaching its goals and ensuring each strategic goal is measurable

   Excellent problem-solving skills – you might not know all the answers, but you know how to find and communicate the solution

   Experience working in some or all of our stack (.NET Core, C#, JavaScript, HTML, Docker, Azure, SQL Server or similar RDBMS, Xamarin)

   Proficiency using source control such as Git, SVN, Subversion, TFS, etc.

   Excitement and enthusiasm for helping customers solve security and password challenges

   Zeal for learning and desire to develop yours, your peers’, and your team’s skills

   NICE-TO-HAVE

   User of Bitwarden

   Startup experience

   Open source experience

   Hands-on software development experience

   Experience leading cross-functional, multi-discipline teams

-}
{-
   I am writing to express my interest in the Software Engineering Manager role. As an ardent admirer of Mercury's mission to redefine the banking experience for startups, I am excited about the opportunity to bring my experience in managing remote Agile teams, my strong software engineering skills, and my commitment to balancing humanity and ROI to your organization.
   As an Engineering Manager at Vendr and Caribou, I led remote teams of engineers to achieve key business outcomes while ensuring a fulfilling work environment. At Caribou, I managed a team of 10 engineers and de-risked a crucial business initiative, resulting in potential gains of over $10M per year. While at Vendr, I created a 'First Team' mindset among fellow Engineering Managers and developed a new internal tool that resulted in significant cost and time savings.
   In my past roles, I have consistently prioritized technical excellence and stability, a testament to my understanding of the high standards that Mercury maintains for its product. I have a strong proficiency in technologies such as Haskell, TypeScript, Elm, and PostgreSQL, in addition to experience with infrastructure technologies like AWS and Docker. The significance of maintaining a careful balance between new developments and existing systems is not lost on me, and this is a principle I've adhered to throughout my career.
   As an engineering leader, I strongly believe in providing a supportive and clear structure for my team members. My past roles have involved regular 1:1s, career growth discussions, coaching, goal setting, feedback, code reviews, and mentoring. I've also had considerable success in conflict resolution and fostering cross-team collaborations, and I always strive to create a positive team culture where every member can thrive.
   Lastly, my experience working in UX at 1Password and developing my engineering skills at ITPro.TV would be beneficial in understanding Mercury's commitment to providing an intuitive, reliable, and beautiful product for its customers.
   I look forward to the opportunity to discuss how I can contribute to your team and help in Mercury's mission to provide startup founders with a banking experience that fuels their progress, rather than hindering it.
-}
