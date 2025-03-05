import { Link } from "@remix-run/react";
import React from "react";
import { Card, Col, Container, Modal, OverlayTrigger, Row, Tooltip } from "react-bootstrap";
import { BookmarkCheckFill, CalendarEvent } from "react-bootstrap-icons";
import { useTranslation } from "react-i18next";
import { MergedHppInfo } from "~/features/slice";

interface HppInfoCardProps {
  hppInfo: MergedHppInfo;
}

const HppInfoCard: React.FC<HppInfoCardProps> = ({ hppInfo }): React.ReactElement => {
  const tagName = (hppInfo.wandbox?.tagName || "------");
  const publishedAt = (hppInfo.wandbox?.publishedAt || "----------").slice(0, 10);
  // wandbox と clangd の情報が同じ場合はオーバーレイを表示しない
  const showRenderTagName = hppInfo.clangd === null || hppInfo.clangd?.tagName === hppInfo.wandbox?.tagName ? false : undefined;
  const renderTagName =
    (props) => {
      return (
        <Tooltip id="button-tooltip" {...props}>
          clangd: {hppInfo.clangd?.tagName}
        </Tooltip>
      );
    };
  const showRenderPublishedAt = hppInfo.clangd === null || hppInfo.clangd?.publishedAt === hppInfo.wandbox?.publishedAt ? false : undefined;
  const renderPublishedAt =
    (props) => {
      return (
        <Tooltip id="button-tooltip" {...props}>
          clangd: {(hppInfo.clangd?.publishedAt || "").slice(0, 10)}
        </Tooltip>
      );
    };
  return (
    <Card className="shadow-sm">
      <Card.Body className="py-4px px-8px">
        {/* タイトル & バージョン */}
        <div className="d-flex justify-content-between align-items-center">
          <Card.Title>
            <Link to={`http://github.com/${hppInfo.repository}`} rel="noreferrer" target="_blank">{hppInfo.name}</Link>
          </Card.Title>

          <div>
            <OverlayTrigger show={showRenderTagName} placement="top" overlay={renderTagName}>
              <small className="text-muted me-8px"><BookmarkCheckFill className="d-inline" /> {tagName}</small>
            </OverlayTrigger>
            <OverlayTrigger show={showRenderPublishedAt} placement="top" overlay={renderPublishedAt}>
              <small className="text-muted"><CalendarEvent className="d-inline" /> {publishedAt}</small>
            </OverlayTrigger>
          </div>
        </div>

        {/* ヘッダ情報 */}
        <Card.Text className="text-primary small mt-1">
          <span className="text-muted fs-6">Header:</span> <code className="fs-5">{hppInfo.headerDescription}</code>
        </Card.Text>

        {/* 説明（More Details で開閉） */}
        <Card.Text className="small text-truncate">
          {hppInfo.description}
        </Card.Text>
      </Card.Body>
    </Card >
  );
}

interface HpplibDialogProps {
  show: boolean;
  hpplib: MergedHppInfo[];
  onHide: () => void;
}
const HpplibDialog: React.FC<HpplibDialogProps> = ({ show, hpplib, onHide }): React.ReactElement => {
  const { t } = useTranslation();
  return (<Modal show={show} onHide={onHide} size="lg">
    <Modal.Header closeButton>
      <Modal.Title className="align-center">{t("compiler.showHpplibDialogDetail")}</Modal.Title>
    </Modal.Header>
    <Modal.Body>
      <Container>
        <Row className="gap-8px">
          {hpplib.map((hppInfo) => (
            <Col md={12} key={hppInfo.name}>
              <HppInfoCard hppInfo={hppInfo} />
            </Col>
          ))}
        </Row>
      </Container>
    </Modal.Body>
  </Modal>);
};

export { HpplibDialog };
