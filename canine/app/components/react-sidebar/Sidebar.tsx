// https://github.com/balloob/react-sidebar を Functional Component で書き直しつつ
// Wandbox で不要なプロパティを削除したりしつつ移植したもの
import React, {
  HTMLAttributes,
  Ref,
  useCallback,
  useEffect,
  useState,
} from "react";

const CANCEL_DISTANCE_ON_SCROLL = 20;

const defaultStyles = {
  root: {
    position: "absolute",
    top: 0,
    left: 0,
    right: 0,
    bottom: 0,
    overflow: "hidden",
  } as React.CSSProperties,
  sidebar: {
    zIndex: 2,
    position: "absolute",
    top: 0,
    bottom: 0,
    transition: "transform .3s ease-out",
    WebkitTransition: "-webkit-transform .3s ease-out",
    willChange: "transform",
    overflowY: "auto",
  } as React.CSSProperties,
  content: {
    position: "absolute",
    top: 0,
    left: 0,
    right: 0,
    bottom: 0,
    overflowY: "auto",
    WebkitOverflowScrolling: "touch",
    transition: "left .3s ease-out, right .3s ease-out",
  } as React.CSSProperties,
  overlay: {
    zIndex: 1,
    position: "fixed",
    top: 0,
    left: 0,
    right: 0,
    bottom: 0,
    opacity: 0,
    visibility: "hidden",
    transition: "opacity .3s ease-out, visibility .3s ease-out",
    backgroundColor: "rgba(0,0,0,.3)",
  } as React.CSSProperties,
};

interface SidebarProps {
  // sidebar content to render
  sidebar: React.ReactElement;

  // styles
  styles?: {
    root?: React.CSSProperties;
    sidebar?: React.CSSProperties;
    content?: React.CSSProperties;
    overlay?: React.CSSProperties;
  };
  // root component optional class
  rootClassName?: string;
  // sidebar optional class
  sidebarClassName?: string;
  // content optional class
  contentClassName?: string;
  // overlay optional class
  overlayClassName?: string;
  // boolean if sidebar should be docked
  docked?: boolean;
  // boolean if sidebar should slide open
  open?: boolean;
  // boolean if transitions should be disabled
  transitions?: boolean;
  // Place the sidebar on the right
  pullRight?: boolean;
  // Initial sidebar width when page loads
  defaultSidebarWidth?: number;

  // root component optional id
  rootId?: string;
  // sidebar optional id
  sidebarId?: string;
  // content optional id
  contentId?: string;
  // overlay optional id
  overlayId?: string;

  // callback called when the overlay is clicked
  onSetOpen?: (open: boolean) => void;
}

const Sidebar: React.FC<SidebarProps> = (props) => {
  const {
    sidebar,
    styles,
    rootClassName,
    sidebarClassName,
    contentClassName,
    overlayClassName,
    docked,
    open,
    transitions,
    pullRight,
    defaultSidebarWidth,
    rootId,
    sidebarId,
    contentId,
    overlayId,
    onSetOpen,
    children,
  } = props;

  const [sidebarWidth, setSidebarWidth] = useState(defaultSidebarWidth);
  const [ref, setRef] = useState<HTMLDivElement | null>(null);

  useEffect(() => {
    if (ref === null) {
      return;
    }
    setSidebarWidth(ref.offsetWidth);
  }, [ref]);

  const sidebarStyle = {
    ...defaultStyles.sidebar,
    ...styles?.sidebar,
  };
  const contentStyle = {
    ...defaultStyles.content,
    ...styles?.content,
  };
  const overlayStyle = {
    ...defaultStyles.overlay,
    ...styles?.overlay,
  };

  const rootProps: HTMLAttributes<HTMLDivElement> = {
    className: rootClassName,
    style: { ...defaultStyles.root, ...styles?.root },
    role: "navigation",
    id: rootId,
  };

  const overlayClicked = useCallback(() => {
    if (open) {
      if (onSetOpen !== undefined) {
        onSetOpen(false);
      }
    }
  }, [open, onSetOpen]);

  if (pullRight) {
    sidebarStyle.right = 0;
    sidebarStyle.transform = "translateX(100%)";
    sidebarStyle.WebkitTransform = "translateX(100%)";
  } else {
    sidebarStyle.left = 0;
    sidebarStyle.transform = "translateX(-100%)";
    sidebarStyle.WebkitTransform = "translateX(-100%)";
  }

  if (docked) {
    // show sidebar
    if (sidebarWidth !== 0) {
      sidebarStyle.transform = `translateX(0%)`;
      sidebarStyle.WebkitTransform = `translateX(0%)`;
    }

    // make space on the left/right side of the content for the sidebar
    if (pullRight) {
      contentStyle.right = `${sidebarWidth}px`;
    } else {
      contentStyle.left = `${sidebarWidth}px`;
    }
  } else if (open) {
    // slide open sidebar
    sidebarStyle.transform = `translateX(0%)`;
    sidebarStyle.WebkitTransform = `translateX(0%)`;

    // show overlay
    overlayStyle.opacity = 1;
    overlayStyle.visibility = "visible";
  }

  if (transitions === false) {
    sidebarStyle.transition = "none";
    sidebarStyle.WebkitTransition = "none";
    contentStyle.transition = "none";
    overlayStyle.transition = "none";
  }

  return (
    <div {...rootProps}>
      <div
        className={sidebarClassName}
        style={sidebarStyle}
        ref={setRef}
        id={sidebarId}
      >
        {sidebar}
      </div>
      <div
        className={overlayClassName}
        style={overlayStyle}
        onClick={overlayClicked}
        id={overlayId}
      />
      <div className={contentClassName} style={contentStyle} id={contentId}>
        {children}
      </div>
    </div>
  );
};

export default Sidebar;
