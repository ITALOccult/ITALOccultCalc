using System.Text;

namespace Occult.Star_Catalogues
{
	internal class OCCstars
	{
		private int oCC;

		private int xZ;

		private int rAH;

		private int rAM;

		private int decD;

		private int decM;

		private int y1;

		private int y2;

		private int y3;

		private int y4;

		private int y5;

		private int m1;

		private int m2;

		private int m3;

		private int m4;

		private int m5;

		private int d1;

		private int d2;

		private int d3;

		private int d4;

		private int d5;

		private int bDZone;

		private int bDNumber;

		private int status = 1;

		private bool decSign;

		private bool bDSign;

		private string discoverer = "";

		private string reference = "";

		private string DSFile = "";

		private string iDCode = "";

		private string iDNumber = "";

		private string iDPair = "";

		private string lightCurve = " ";

		public int OCC
		{
			get
			{
				return oCC;
			}
			set
			{
				oCC = value;
			}
		}

		public int XZ
		{
			get
			{
				return xZ;
			}
			set
			{
				xZ = value;
			}
		}

		public int Status
		{
			get
			{
				return status;
			}
			set
			{
				status = value;
			}
		}

		public int RAH
		{
			get
			{
				return rAH;
			}
			set
			{
				rAH = value;
			}
		}

		public int RAM
		{
			get
			{
				return rAM;
			}
			set
			{
				rAM = value;
			}
		}

		public bool DecSign
		{
			get
			{
				return decSign;
			}
			set
			{
				decSign = value;
			}
		}

		public int DecD
		{
			get
			{
				return decD;
			}
			set
			{
				decD = value;
			}
		}

		public int DecM
		{
			get
			{
				return decM;
			}
			set
			{
				decM = value;
			}
		}

		public string Discoverer
		{
			get
			{
				return discoverer;
			}
			set
			{
				discoverer = value;
			}
		}

		public string Reference
		{
			get
			{
				return reference;
			}
			set
			{
				reference = value;
			}
		}

		public int Y1
		{
			get
			{
				return y1;
			}
			set
			{
				y1 = value;
			}
		}

		public int M1
		{
			get
			{
				return m1;
			}
			set
			{
				m1 = value;
			}
		}

		public int D1
		{
			get
			{
				return d1;
			}
			set
			{
				d1 = value;
			}
		}

		public int Y2
		{
			get
			{
				return y2;
			}
			set
			{
				y2 = value;
			}
		}

		public int M2
		{
			get
			{
				return m2;
			}
			set
			{
				m2 = value;
			}
		}

		public int D2
		{
			get
			{
				return d2;
			}
			set
			{
				d2 = value;
			}
		}

		public int Y3
		{
			get
			{
				return y3;
			}
			set
			{
				y3 = value;
			}
		}

		public int M3
		{
			get
			{
				return m3;
			}
			set
			{
				m3 = value;
			}
		}

		public int D3
		{
			get
			{
				return d3;
			}
			set
			{
				d3 = value;
			}
		}

		public int Y4
		{
			get
			{
				return y4;
			}
			set
			{
				y4 = value;
			}
		}

		public int M4
		{
			get
			{
				return m4;
			}
			set
			{
				m4 = value;
			}
		}

		public int D4
		{
			get
			{
				return d4;
			}
			set
			{
				d4 = value;
			}
		}

		public int Y5
		{
			get
			{
				return y5;
			}
			set
			{
				y5 = value;
			}
		}

		public int M5
		{
			get
			{
				return m5;
			}
			set
			{
				m5 = value;
			}
		}

		public int D5
		{
			get
			{
				return d5;
			}
			set
			{
				d5 = value;
			}
		}

		public bool BDSign
		{
			get
			{
				return bDSign;
			}
			set
			{
				bDSign = value;
			}
		}

		public int BDZone
		{
			get
			{
				return bDZone;
			}
			set
			{
				bDZone = value;
			}
		}

		public int BDNumber
		{
			get
			{
				return bDNumber;
			}
			set
			{
				bDNumber = value;
			}
		}

		public string IDCode
		{
			get
			{
				return iDCode;
			}
			set
			{
				iDCode = value;
			}
		}

		public string IDNumber
		{
			get
			{
				return iDNumber;
			}
			set
			{
				iDNumber = value;
			}
		}

		public string IDPair
		{
			get
			{
				return iDPair;
			}
			set
			{
				iDPair = value;
			}
		}

		public string dsFile
		{
			get
			{
				return DSFile;
			}
			set
			{
				DSFile = value;
			}
		}

		public string LightCurve
		{
			get
			{
				return lightCurve.PadRight(1).Substring(0, 1);
			}
			set
			{
				lightCurve = value;
			}
		}

		public void DecodeLine(string InLine)
		{
			if (!int.TryParse(InLine.Substring(3, 4), out oCC))
			{
				oCC = 0;
			}
			if (!int.TryParse(InLine.Substring(7, 6), out xZ))
			{
				xZ = -1;
			}
			if (!int.TryParse(InLine.Substring(14, 2), out rAH))
			{
				rAH = 0;
			}
			if (!int.TryParse(InLine.Substring(16, 3), out rAM))
			{
				rAM = 0;
			}
			decSign = !(InLine.Substring(19, 1) == "-");
			if (!int.TryParse(InLine.Substring(20, 2), out decD))
			{
				decD = 0;
			}
			if (!int.TryParse(InLine.Substring(22, 2), out decM))
			{
				decM = 0;
			}
			if (!int.TryParse(InLine.Substring(25, 4), out y1))
			{
				y1 = 0;
			}
			if (!int.TryParse(InLine.Substring(29, 2), out m1))
			{
				m1 = 0;
			}
			if (!int.TryParse(InLine.Substring(31, 2), out d1))
			{
				d1 = 0;
			}
			discoverer = InLine.Substring(34, 24);
			reference = InLine.Substring(59, 24);
			if (!int.TryParse(InLine.Substring(84, 4), out y2))
			{
				y2 = 0;
			}
			if (!int.TryParse(InLine.Substring(88, 2), out m2))
			{
				m2 = 0;
			}
			if (!int.TryParse(InLine.Substring(90, 2), out d2))
			{
				d2 = 0;
			}
			if (!int.TryParse(InLine.Substring(93, 4), out y3))
			{
				y3 = 0;
			}
			if (!int.TryParse(InLine.Substring(97, 2), out m3))
			{
				m3 = 0;
			}
			if (!int.TryParse(InLine.Substring(99, 2), out d3))
			{
				d3 = 0;
			}
			if (!int.TryParse(InLine.Substring(102, 4), out y4))
			{
				y4 = 0;
			}
			if (!int.TryParse(InLine.Substring(106, 2), out m4))
			{
				m4 = 0;
			}
			if (!int.TryParse(InLine.Substring(108, 2), out d4))
			{
				d4 = 0;
			}
			if (!int.TryParse(InLine.Substring(111, 4), out y5))
			{
				y5 = 0;
			}
			if (!int.TryParse(InLine.Substring(115, 2), out m5))
			{
				m5 = 0;
			}
			if (!int.TryParse(InLine.Substring(117, 2), out d5))
			{
				d5 = 0;
			}
			lightCurve = InLine.Substring(119, 1);
			if (!" ?*".Contains(lightCurve))
			{
				lightCurve = " ";
			}
			iDCode = InLine.Substring(120, 3);
			iDNumber = InLine.Substring(123, 4);
			iDPair = InLine.Substring(128, 5);
			bDSign = !(InLine.Substring(133, 1) == "-");
			if (!int.TryParse(InLine.Substring(134, 2), out bDZone))
			{
				bDZone = -99;
			}
			if (!int.TryParse(InLine.Substring(136, 5), out bDNumber))
			{
				bDNumber = -1;
			}
			DSFile = InLine.Substring(142, 7);
			status = "APE".IndexOf(InLine.Substring(13, 1));
			if (status < 0)
			{
				if (DSFile.Contains("single"))
				{
					Status = 2;
				}
				else if (iDCode.Trim().Length > 0)
				{
					Status = 0;
				}
				else
				{
					status = 1;
				}
			}
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("OCC{0,4:F0}", oCC);
			stringBuilder.AppendFormat("{0,6:F0}", xZ);
			stringBuilder.Append("APE".Substring(status, 1));
			stringBuilder.Append(rAH.ToString().PadLeft(2, '0') + rAM.ToString().PadLeft(3, '0'));
			if (!decSign)
			{
				stringBuilder.Append("-");
			}
			else
			{
				stringBuilder.Append("+");
			}
			stringBuilder.Append(decD.ToString().PadLeft(2, '0') + decM.ToString().PadLeft(2, '0'));
			stringBuilder.Append(" " + y1.ToString().PadLeft(4) + m1.ToString().PadLeft(2) + d1.ToString().PadLeft(2));
			stringBuilder.Append(" " + discoverer.PadRight(24).Substring(0, 24));
			stringBuilder.Append(" " + reference.PadRight(24).Substring(0, 24));
			if (y2 > 0)
			{
				stringBuilder.Append(" " + y2.ToString().PadLeft(4) + m2.ToString().PadLeft(2) + d2.ToString().PadLeft(2));
			}
			else
			{
				stringBuilder.Append("".PadRight(9));
			}
			if (y3 > 0)
			{
				stringBuilder.Append(" " + y3.ToString().PadLeft(4) + m3.ToString().PadLeft(2) + d3.ToString().PadLeft(2));
			}
			else
			{
				stringBuilder.Append("".PadRight(9));
			}
			if (y4 > 0)
			{
				stringBuilder.Append(" " + y4.ToString().PadLeft(4) + m4.ToString().PadLeft(2) + d4.ToString().PadLeft(2));
			}
			else
			{
				stringBuilder.Append("".PadRight(9));
			}
			if (y5 > 0)
			{
				stringBuilder.Append(" " + y5.ToString().PadLeft(4) + m5.ToString().PadLeft(2) + d5.ToString().PadLeft(2));
			}
			else
			{
				stringBuilder.Append("".PadRight(9));
			}
			stringBuilder.Append(LightCurve + iDCode.Trim().PadRight(3) + iDNumber.PadRight(4) + " " + iDPair.Trim().PadRight(5));
			if (bDZone > -90)
			{
				if (!bDSign)
				{
					stringBuilder.Append("-");
				}
				else
				{
					stringBuilder.Append("+");
				}
				stringBuilder.Append(bDZone.ToString().PadLeft(2, '0'));
				stringBuilder.Append(bDNumber.ToString().PadLeft(5));
			}
			else
			{
				stringBuilder.Append("".PadRight(8));
			}
			stringBuilder.Append(" " + DSFile.PadRight(7).Substring(0, 7));
			return stringBuilder.ToString();
		}
	}
}
