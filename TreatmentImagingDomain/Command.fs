module Command

open Model


type Result<'t> =
| Ok of 't
| Invalid of string


// QUERY operation
let getReferenceImageForField (txField:TreatmentField) =
    Seq.filter 
        (fun image ->
            image.Association = FieldAssociated txField.Id
                && image.IsReferenceImageForField)
    >> Seq.tryHead


// QUERY operation
let isImageReviewNeeded (images:Image list) 
        (offsets:Offset list) =

    false


// QUERY operation
let isTrendReviewNeeded (treatmentSessions:TreatmentSession list) =

    false


// QUERY operation
let determineLocalizationOffset (treatmentSessions:TreatmentSession list) =

    [| |]




let scaleImage (image:Image) 
        (pixelSpacing:decimal) =

    match image.OffsetId with
    | None ->
        Ok { image with PixelSpacing = pixelSpacing }
    | _ ->
        Invalid "can't have offset"

let associateImageToSite (image:Image) 
        (txSite:TreatmentSite) 
        (offsets:Offset list) =

    match (image.Association, image.OffsetId) with
    | PatientAssociated, None ->
        Ok ({ image with Association = SiteAssociated txSite.Id }) 
    | _ -> 
        Invalid "image must be patient associated with no offset"


let associateImageToField (image:Image) 
        (txField:TreatmentField)
        (allImages:Image list) =

    match (image.Association, image.OffsetId) with
    | PatientAssociated, None ->
        let Some(referenceImage) = 
            allImages
            |> getReferenceImageForField txField
        Ok { image with Association = FieldAssociated txField.Id }
    | _ -> 
        Invalid "image must be patient associated with no offset"


let setAsReferenceForField (image:Image) 
        (txField:TreatmentField)
        (allImages:Image list) =

    match (image.Association, image.OffsetId) with
    | PatientAssociated, None ->
        Ok { image with Association = FieldAssociated txField.Id }
    | _ -> 
        Invalid "image must be patient associated with no offset"


let associateThirdPartyOffset (image:Image) 
        (thirdPartyOffset:Offset) =

    match (image.Association, image.OffsetId) with
    | FieldAssociated _, None ->
        Ok ({ image with OffsetId = Some(thirdPartyOffset.OffsetId) }, 
            thirdPartyOffset)
    | _ -> 
        Invalid "image must be field associated with no offset"


let associateStereoPartner (image1:Image) 
        (image2:Image) =

    match (image1.Association, image2.Association) with
    | FieldAssociated _, FieldAssociated _ ->
        Ok ({ image1 with StereoPartnerId = Some(image2.Id) },
            { image2 with StereoPartnerId = Some(image1.Id) })
    | _ ->
        Invalid ""


let registerImageToReference (image:Image) 
        (offset:Offset option) 
        (offsetX:decimal) = 

    match (image.OffsetId, offset) with
    | Some(offsetId), Some(offset) when offsetId = offset.OffsetId ->
        Ok { offset with ShiftX = offsetX }

    | None, None ->
        Ok Unchecked.defaultof<Offset>  // construct a new offset, and associate the image

    | _ -> 
        Invalid ""


let invalidateOffset (offset:Offset) = 

    { offset with OffsetState = InProgress }


let statusImage (image:Image) 
        (offset:Offset) 
        (status:ImageStatus) =

    Ok ({ image with Status = status }, offset)


let updateSiteLocalizationOffset (txSite:TreatmentSite) 
        (offset:decimal) =

    (txSite, offset)


type ITreatmentImagingDomainContext =
    abstract member TreatmentSite : TreatmentSite
    abstract member PatientSetup : PatientSetup
    abstract member TreatmentFields : TreatmentField seq
    abstract member TreatmentSessions : TreatmentSession seq
    abstract member Images : Image seq
    abstract member Offsets : Offset seq

type TreatmentImagingCommand =
    | ScaleImage of imageId:EntityId * scale:decimal
    | AssociateImageToSite of siteId:EntityId * imageId:EntityId
    | AssociateImageToField of fieldId:EntityId * imageId:EntityId

let doCommand (command:TreatmentImagingCommand) (context:ITreatmentImagingDomainContext) =

    match command with
    | ScaleImage (imageId, scale) ->
        context.Images 
        |> Seq.tryFind
            (fun image -> image.Id = imageId)
        |> function 
            | Some(image) ->
                let scaledImage = scaleImage image scale
                Unchecked.defaultof<ITreatmentImagingDomainContext>
                |> Ok
            | None -> Invalid ""

    | _ -> Invalid ""
