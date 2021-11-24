module AltModel

open System

type EntityId = Guid


type Result<'t> =
    | Ok of 't
    | Invalid of InvalidReason
and InvalidReason =
    | CantAssociateWithoutPixelSpacing
    | GantryAnglesDontMatch
    | NoReferenceImage


type Site = 
    { Id : EntityId }

type Field =
    { Id : EntityId;
        Gantry : decimal }

type Image =
    { Id : EntityId;
        AcquisitionDateTime : DateTime;
        Gantry : decimal option;
        PixelSpacing : decimal option }

type AssociatedImage =
    | Site of Site * Image
    | Field of Field * Image
    | Patient of EntityId * Image       // is this really used?  for approval

type AssociatedStereoPair =
    | Site of AssociatedImage * AssociatedImage
    | Field of AssociatedImage * AssociatedImage


type StatusedEntity<'TAssociatedOrRegistered> =
    { Status: Status;
        Approver: EntityId;
        StatusDateTime: DateTime;
        Entity: 'TAssociatedOrRegistered }
and Status =
    | Approved of ApprovalPurpose
    | Rejected of RejectionReason
    | NotRequired
and ApprovalPurpose =
    | ReferenceImage
    | TrendCalculation
and RejectionReason =
    | JustBecause



type Transformation = 
    { OffsetX:decimal }

type RegisteredImageSet<'TAssociated> =
    { LocalizationImageSet : 'TAssociated;
        ReferenceImageSet : 'TAssociated;
        Transformation : Transformation }


let associateImageToSite (site:Site) (image:Image) =

    match image with
    | { PixelSpacing = Some(_) } ->
        (site, image)
        |> AssociatedImage.Site 
        |> Ok
    | _ -> 
        CantAssociateWithoutPixelSpacing
        |> Invalid


let associatePairToSite (site:Site) (image1:Image) (image2:Image) =

    match image1, image2 with
    | { PixelSpacing = Some(_) }, 
            { PixelSpacing = Some(_) }->
        ((site, image1) 
            |> AssociatedImage.Site, 
            (site, image2) 
            |> AssociatedImage.Site)
        |> AssociatedStereoPair.Site 
        |> Ok
    | _ -> 
        CantAssociateWithoutPixelSpacing
        |> Invalid
    
    

let associateImageToField (field:Field) (image:Image) =

    match field.Gantry, image with
    | fieldGantry, 
            { PixelSpacing = Some(_); 
                Gantry=Some imageGantry } 
            when fieldGantry = imageGantry ->

        (field, image)
        |> AssociatedImage.Field 
        |> Ok

    | _ -> 
        GantryAnglesDontMatch
        |> Invalid 


let associatePairToFields (field1:Field) (image1:Image) (field2:Field) (image2:Image) =

    // check image times match

    match (associateImageToField field1 image1,
            associateImageToField field2 image2,
            image1.AcquisitionDateTime - image2.AcquisitionDateTime) with 
    | Ok fieldAssociatedImage1, Ok fieldAssociatedImage2,
        timeOffset when timeOffset.TotalMinutes < 10.0 ->

        (fieldAssociatedImage1,
            fieldAssociatedImage2)
        |> AssociatedStereoPair.Field
        |> Ok

    | _ -> 
        GantryAnglesDontMatch
        |> Invalid


let statusAssociatedEntity<'TAssociated> () = 
    ()

let getReferenceImageForField (field:Field) 
        (allStatusedImages:StatusedEntity<AssociatedImage> list) =

    allStatusedImages
    |> Seq.choose
        (fun approvedEntity ->
            match approvedEntity with
            | { Status = Approved ReferenceImage;
                    StatusDateTime = whenApproved;
                    Entity = AssociatedImage.Field (field, image) } -> 

                (whenApproved, 
                    (field, image)
                    |> AssociatedImage.Field)
                |> Some

            | _ -> None)
    |> Seq.sort
    |> Seq.tryHead
    |> function
        | Some(_, associatedImage) -> 
            associatedImage 
            |> Ok
        | None -> 
            NoReferenceImage
            |> Invalid 

let getReferenceStereoPairForFields field1 field2 allImages =

    match getReferenceImageForField field1 allImages, 
                getReferenceImageForField field2 allImages with
    | Ok fieldAssociatedImage1, 
        Ok fieldAssociatedImage2 ->
            (fieldAssociatedImage1, 
                fieldAssociatedImage2)
            |> AssociatedStereoPair.Field 
            |> Ok
    | _ -> 
        NoReferenceImage
        |> Invalid

let associateToFieldWithInitialRegistration field image allImages =

    match (associateImageToField field image,
            getReferenceImageForField field allImages) with

    | Ok associatedImage, Ok associatedReference ->

        { LocalizationImageSet = associatedImage;
            ReferenceImageSet = associatedReference;
            Transformation = { OffsetX = 0.0m } }
        |> Ok

    | _ -> 
        NoReferenceImage
        |> Invalid

let getTransformation<'TAssociated> (ris:RegisteredImageSet<'TAssociated>) =
    ris.Transformation




type TreatmentImagingSession =
    { SessionNumber:int;
        AssociatedImages: AssociatedImage list;
        RegisteredImages: RegisteredImageSet<AssociatedImage> list;
        StatusedImages: StatusedEntity<AssociatedImage> list;

        AssociatedStereoPairs: AssociatedStereoPair list;
        RegisteredStereoPairs: RegisteredImageSet<AssociatedStereoPair> list;
        StatusedAssociatedStereoPairs: StatusedEntity<AssociatedStereoPair> list; }

type TrendingProtocol =
    { SessionCount: int }

type TrendingResult =
    | NotEnoughSessions of Transformation
    | ApplyLocalization of Transformation
    | ValidateOnly of Transformation

//let trendingForSessions<'TAssociated> (protocol:TrendingProtocol) 
//        (forSessions: TreatmentImagingSession list) =

//    let sessionNumberTransforms =
//        forSessions
//        |> List.choose 
//            (fun session -> 
//                match session.RegisteredImages |> List.head with
//                | _ -> 
//                    Some(session.SessionNumber, getTransformation)
//                | _  -> None)
//        |> List.sort

//    NotEnoughSessions { OffsetX = 0.0m }


type ImagingDomain =
    { Sites: Site list;
        Fields: Field list;
        UnassociatedImages: Image list;

        AssociatedImages: AssociatedImage list;
        RegisteredImages: RegisteredImageSet<AssociatedImage> list;
        StatusedImages: StatusedEntity<AssociatedImage> list;

        AssociatedStereoPairs: AssociatedStereoPair list;
        RegisteredStereoPairs: RegisteredImageSet<AssociatedStereoPair> list;
        StatusedAssociatedStereoPairs: StatusedEntity<AssociatedStereoPair> list;

        Sessions:TreatmentImagingSession list; }
 