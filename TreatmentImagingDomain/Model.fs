module Model

open System

type EntityId = Guid

type TreatmentSite =
    { Id:EntityId;
        PatientSetupId:EntityId }

type PatientSetup =
    { Id:EntityId;
        PatientPosition:int;
        IsocenterPosition:int;
        PrescribedRelativeOffsetId:EntityId;
        LocalizationOffset:EntityId }

type TreatmentField =
    { Id:EntityId;
        ControlPoints:ControlPoint list}
and ControlPoint =
    { Index:int;
        GantryAngle:decimal }

type TreatmentSession =
    { SessionNumber:int;
        ImageIds:EntityId list;
        OffsetIds:EntityId list; }

type Image =
    { Id:EntityId;
        Association:ImageAssociation;
        IsReferenceImageForField:bool;
        ReferenceImageId:EntityId option;
        OffsetId:EntityId option;
        StereoPartnerId:EntityId option;
        AcquisitionDateTime:DateTime;
        PixelSpacing:decimal;
        Status:ImageStatus }
and ImageAssociation =
| SiteAssociated of EntityId
| FieldAssociated of EntityId
| PatientAssociated
and ImageStatus =
| NotReviewed
| Approved
| Rejected
| NotRequired

type Offset =
    { OffsetId:EntityId;
        OffsetType:OffsetType;
        OffsetState:OffsetState;
        ShiftX:decimal }
and OffsetType =
    | Localization
    | Image
and OffsetState =
    | InProgress
