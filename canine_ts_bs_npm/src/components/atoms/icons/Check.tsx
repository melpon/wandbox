import React from "react";

import { Icon, IconProps } from "./Icon";

const Check: React.FC<IconProps> = (props): React.ReactElement => {
  return (
    <Icon {...props} className="bi-check">
      <path
        fillRule="evenodd"
        d="M10.97 4.97a.75.75 0 0 1 1.071 1.05l-3.992 4.99a.75.75 0 0 1-1.08.02L4.324 8.384a.75.75 0 1 1 1.06-1.06l2.094 2.093 3.473-4.425a.236.236 0 0 1 .02-.022z"
      />
    </Icon>
  );
};
export default Check;
