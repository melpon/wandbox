import React from "react";

export interface IconProps {
  color?: string;
  size?: string | number;
}

export interface InternalIconProps extends IconProps {
  className: string;
}
const Icon: React.FC<InternalIconProps> = (props): React.ReactElement => {
  const size = props.size === undefined ? "1em" : props.size;
  const color = props.color === undefined ? "currentColor" : props.color;
  return (
    <svg
      className={`bi ${props.className}`}
      width={size}
      height={size}
      viewBox="0 0 16 16"
      fill={color}
      xmlns="http://www.w3.org/2000/svg"
    >
      {props.children}
    </svg>
  );
};
export { Icon };
