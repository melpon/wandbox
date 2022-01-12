import React from "react";
import { Link } from "react-router-dom";
import Navbar from "react-bootstrap/Navbar";

// eslint-disable-next-line @typescript-eslint/no-empty-interface
interface HeaderProps {}

const Header: React.FC<HeaderProps> = (): React.ReactElement => {
  return (
    <Navbar>
      <Navbar.Brand as={Link} to="/">
        Wandbox
      </Navbar.Brand>
    </Navbar>
  );
};

export { Header };
