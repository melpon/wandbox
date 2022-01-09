import React from "react";
import Link from 'next/link'
import Navbar from "react-bootstrap/Navbar";

// eslint-disable-next-line @typescript-eslint/no-empty-interface
interface HeaderProps {}

const Header: React.FC<HeaderProps> = (): React.ReactElement => {
  return (
    <Navbar>
      <Navbar.Brand as={Link} href="/">
        Wandbox
      </Navbar.Brand>
    </Navbar>
  );
};

export { Header };
