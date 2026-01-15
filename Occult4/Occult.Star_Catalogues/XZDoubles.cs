using System;
using System.Text;

namespace Occult.Star_Catalogues
{
	internal class XZDoubles : IComparable
	{
		private int xZPrimary;

		private int xZSecondary;

		private int xZPrinciple;

		private int rAH;

		private int rAM;

		private int decD;

		private int decM;

		private int pMRA;

		private int pMDec;

		private int bDZone;

		private int bDNumber;

		private int y1;

		private int y2;

		private int numOfObs;

		private bool decSign;

		private bool bDSign;

		private bool sep1_LessThan;

		private bool sep2_LessThan;

		private bool meanPosn;

		private bool lineAdded;

		private double rA_hours;

		private double dEC_Degrees;

		private double pA1;

		private double pA2;

		private double sep1;

		private double sep2;

		private double mag1;

		private double mag2;

		private int pA1_Places;

		private int pA2_Places;

		private int sep1_Places;

		private int sep2_Places;

		private int mag1_Places;

		private int mag2_Places;

		private double period;

		private double axis;

		private double inclination;

		private double node;

		private double t;

		private double e;

		private double periastron;

		private int period_Places;

		private int axis_Places;

		private int inclination_Places;

		private int node_Places;

		private int t_Places;

		private int e_Places;

		private int periastron_Places;

		private string iDCode;

		private string iDNumber;

		private string iDPair;

		private string spectrum;

		private string wDSNote1;

		private string wDSNote2;

		private string wDSNote3;

		public int XZPrimary
		{
			get
			{
				return xZPrimary;
			}
			set
			{
				xZPrimary = value;
			}
		}

		public int XZSecondary
		{
			get
			{
				return xZSecondary;
			}
			set
			{
				xZSecondary = value;
			}
		}

		public int XZPrinciple
		{
			get
			{
				return xZPrinciple;
			}
			set
			{
				xZPrinciple = value;
			}
		}

		public bool MeanPosn
		{
			get
			{
				return meanPosn;
			}
			set
			{
				meanPosn = value;
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

		public double RA_Hours
		{
			get
			{
				return rA_hours;
			}
			set
			{
				rA_hours = value;
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

		public double Dec_Degrees
		{
			get
			{
				return dEC_Degrees;
			}
			set
			{
				dEC_Degrees = value;
			}
		}

		public int PMRA
		{
			get
			{
				return pMRA;
			}
			set
			{
				pMRA = value;
			}
		}

		public int PMDec
		{
			get
			{
				return pMDec;
			}
			set
			{
				pMDec = value;
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

		public int NumOfObs
		{
			get
			{
				return numOfObs;
			}
			set
			{
				numOfObs = value;
			}
		}

		public double PA1
		{
			get
			{
				return pA1;
			}
			set
			{
				pA1 = value;
			}
		}

		public int PA1_Places
		{
			get
			{
				return pA1_Places;
			}
			set
			{
				pA1_Places = value;
			}
		}

		public double PA2
		{
			get
			{
				return pA2;
			}
			set
			{
				pA2 = value;
			}
		}

		public int PA2_Places
		{
			get
			{
				return pA2_Places;
			}
			set
			{
				pA2_Places = value;
			}
		}

		public double Sep1
		{
			get
			{
				return sep1;
			}
			set
			{
				sep1 = value;
			}
		}

		public int Sep1_Places
		{
			get
			{
				return sep1_Places;
			}
			set
			{
				sep1_Places = value;
			}
		}

		public bool Sep1_LessThan
		{
			get
			{
				return sep1_LessThan;
			}
			set
			{
				sep1_LessThan = value;
			}
		}

		public double Sep2
		{
			get
			{
				return sep2;
			}
			set
			{
				sep2 = value;
			}
		}

		public int Sep2_Places
		{
			get
			{
				return sep2_Places;
			}
			set
			{
				sep2_Places = value;
			}
		}

		public bool Sep2_LessThan
		{
			get
			{
				return sep2_LessThan;
			}
			set
			{
				sep2_LessThan = value;
			}
		}

		public double Mag1
		{
			get
			{
				return mag1;
			}
			set
			{
				mag1 = value;
			}
		}

		public int Mag1_Places
		{
			get
			{
				return mag1_Places;
			}
			set
			{
				mag1_Places = value;
			}
		}

		public double Mag2
		{
			get
			{
				return mag2;
			}
			set
			{
				mag2 = value;
			}
		}

		public int Mag2_Places
		{
			get
			{
				return mag2_Places;
			}
			set
			{
				mag2_Places = value;
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

		public string Spectrum
		{
			get
			{
				return spectrum;
			}
			set
			{
				spectrum = value;
			}
		}

		public string WDSNote1
		{
			get
			{
				return wDSNote1;
			}
			set
			{
				wDSNote1 = value;
			}
		}

		public string WDSNote2
		{
			get
			{
				return wDSNote2;
			}
			set
			{
				wDSNote2 = value;
			}
		}

		public string WDSNote3
		{
			get
			{
				return wDSNote3;
			}
			set
			{
				wDSNote3 = value;
			}
		}

		public bool LineAdded
		{
			get
			{
				return lineAdded;
			}
			set
			{
				lineAdded = value;
			}
		}

		public double Period
		{
			get
			{
				return period;
			}
			set
			{
				period = value;
			}
		}

		public int Period_Places
		{
			get
			{
				return period_Places;
			}
			set
			{
				period_Places = value;
			}
		}

		public double Axis
		{
			get
			{
				return axis;
			}
			set
			{
				axis = value;
			}
		}

		public int Axis_Places
		{
			get
			{
				return axis_Places;
			}
			set
			{
				axis_Places = value;
			}
		}

		public double Inclination
		{
			get
			{
				return inclination;
			}
			set
			{
				inclination = value;
			}
		}

		public int Inclination_Places
		{
			get
			{
				return inclination_Places;
			}
			set
			{
				inclination_Places = value;
			}
		}

		public double Node
		{
			get
			{
				return node;
			}
			set
			{
				node = value;
			}
		}

		public int Node_Places
		{
			get
			{
				return node_Places;
			}
			set
			{
				node_Places = value;
			}
		}

		public double T
		{
			get
			{
				return t;
			}
			set
			{
				t = value;
			}
		}

		public int T_Places
		{
			get
			{
				return t_Places;
			}
			set
			{
				t_Places = value;
			}
		}

		public double E
		{
			get
			{
				return e;
			}
			set
			{
				e = value;
			}
		}

		public int E_Places
		{
			get
			{
				return e_Places;
			}
			set
			{
				e_Places = value;
			}
		}

		public double Periastron
		{
			get
			{
				return periastron;
			}
			set
			{
				periastron = value;
			}
		}

		public int Periastron_Places
		{
			get
			{
				return periastron_Places;
			}
			set
			{
				periastron_Places = value;
			}
		}

		public int RA => 1000 * rAH + rAM;

		public int Dec => GetDec();

		internal int GetDec()
		{
			if (decSign)
			{
				return 100 * decD + decM;
			}
			return -(100 * decD + decM);
		}

		public void DecodeLine(string InLine)
		{
			if (!int.TryParse(InLine.Substring(0, 6), out xZPrimary))
			{
				xZPrimary = -1;
			}
			if (!int.TryParse(InLine.Substring(7, 6), out xZSecondary))
			{
				xZSecondary = -1;
			}
			if (!int.TryParse(InLine.Substring(14, 6), out xZPrinciple))
			{
				xZPrinciple = -1;
			}
			meanPosn = InLine.Substring(20, 1) == "M";
			if (!int.TryParse(InLine.Substring(21, 2), out rAH))
			{
				rAH = 0;
			}
			if (!int.TryParse(InLine.Substring(23, 3), out rAM))
			{
				rAM = 0;
			}
			RA_Hours = (double)rAH + (double)rAM / 600.0;
			decSign = !(InLine.Substring(26, 1) == "-");
			if (!int.TryParse(InLine.Substring(27, 2), out decD))
			{
				decD = 0;
			}
			if (!int.TryParse(InLine.Substring(29, 2), out decM))
			{
				decM = 0;
			}
			Dec_Degrees = (double)decD + (double)decM / 60.0;
			if (InLine.Substring(26, 1) == "-")
			{
				Dec_Degrees = 0.0 - Dec_Degrees;
			}
			iDCode = InLine.Substring(32, 3);
			iDNumber = InLine.Substring(35, 4);
			iDPair = InLine.Substring(40, 5);
			if (!int.TryParse(InLine.Substring(46, 4), out y1))
			{
				y1 = -1;
			}
			if (!int.TryParse(InLine.Substring(51, 4), out y2))
			{
				y2 = -1;
			}
			if (!int.TryParse(InLine.Substring(56, 2), out numOfObs))
			{
				numOfObs = -1;
			}
			if (!double.TryParse(InLine.Substring(59, 5), out pA1))
			{
				pA1 = -1.0;
			}
			else
			{
				pA1_Places = Utilities.DecimalPlaces(InLine.Substring(59, 5));
			}
			if (!double.TryParse(InLine.Substring(65, 5), out pA2))
			{
				pA2 = -1.0;
			}
			else
			{
				pA2_Places = Utilities.DecimalPlaces(InLine.Substring(65, 5));
			}
			sep1_LessThan = InLine.Substring(71, 7).Contains("<");
			if (!double.TryParse(InLine.Substring(71, 7).Replace("<", ""), out sep1))
			{
				sep1 = -1.0;
			}
			else
			{
				sep1_Places = Utilities.DecimalPlaces(InLine.Substring(71, 7).Replace("<", ""));
			}
			sep2_LessThan = InLine.Substring(79, 7).Contains("<");
			if (!double.TryParse(InLine.Substring(79, 7).Replace("<", ""), out sep2))
			{
				sep2 = -1.0;
			}
			else
			{
				sep2_Places = Utilities.DecimalPlaces(InLine.Substring(79, 7).Replace("<", ""));
			}
			if (!double.TryParse(InLine.Substring(87, 5), out mag1))
			{
				mag1 = -10.0;
			}
			else
			{
				mag1_Places = Utilities.DecimalPlaces(InLine.Substring(87, 5));
			}
			if (!double.TryParse(InLine.Substring(93, 5), out mag2))
			{
				mag2 = -10.0;
			}
			else
			{
				mag2_Places = Utilities.DecimalPlaces(InLine.Substring(93, 5));
			}
			spectrum = InLine.Substring(99, 10);
			if (!int.TryParse(InLine.Substring(109, 4), out pMRA))
			{
				pMRA = -999;
			}
			if (!int.TryParse(InLine.Substring(114, 4), out pMDec))
			{
				pMDec = -999;
			}
			bDSign = !(InLine.Substring(119, 1) == "-");
			if (!int.TryParse(InLine.Substring(120, 2), out bDZone))
			{
				bDZone = -99;
			}
			if (!int.TryParse(InLine.Substring(122, 5), out bDNumber))
			{
				bDNumber = -1;
			}
			wDSNote1 = InLine.Substring(128, 1);
			wDSNote2 = InLine.Substring(129, 1);
			wDSNote3 = InLine.Substring(130, 1);
			if (!double.TryParse(InLine.Substring(132, 11), out period))
			{
				period = -1.0;
			}
			else
			{
				period_Places = Utilities.DecimalPlaces(InLine.Substring(131, 12));
			}
			if (!double.TryParse(InLine.Substring(143, 9), out axis))
			{
				axis = -1.0;
			}
			else
			{
				axis_Places = Utilities.DecimalPlaces(InLine.Substring(143, 9));
			}
			if (!double.TryParse(InLine.Substring(153, 8), out inclination))
			{
				inclination = -1.0;
			}
			else
			{
				inclination_Places = Utilities.DecimalPlaces(InLine.Substring(153, 8));
			}
			if (!double.TryParse(InLine.Substring(162, 8), out node))
			{
				node = -1.0;
			}
			else
			{
				node_Places = Utilities.DecimalPlaces(InLine.Substring(162, 8));
			}
			if (!double.TryParse(InLine.Substring(171, 10), out t))
			{
				t = -1.0;
			}
			else
			{
				t_Places = Utilities.DecimalPlaces(InLine.Substring(171, 10));
			}
			if (!double.TryParse(InLine.Substring(182, 7), out e))
			{
				e = -1.0;
			}
			else
			{
				e_Places = Utilities.DecimalPlaces(InLine.Substring(182, 7));
			}
			if (!double.TryParse(InLine.Substring(190, 8), out periastron))
			{
				periastron = -1.0;
			}
			else
			{
				periastron_Places = Utilities.DecimalPlaces(InLine.Substring(190, 8));
			}
		}

		public void Decode_WDS2006_Line(string InLine)
		{
			if (!int.TryParse(InLine.Substring(0, 2), out rAH))
			{
				rAH = 0;
			}
			if (!int.TryParse(InLine.Substring(2, 3), out rAM))
			{
				rAM = 0;
			}
			decSign = !(InLine.Substring(5, 1) == "-");
			if (!int.TryParse(InLine.Substring(6, 2), out decD))
			{
				decD = 0;
			}
			if (!int.TryParse(InLine.Substring(8, 2), out decM))
			{
				decM = 0;
			}
			iDCode = InLine.Substring(10, 3);
			iDNumber = InLine.Substring(13, 4);
			iDPair = InLine.Substring(17, 5);
			if (!int.TryParse(InLine.Substring(23, 4), out y1))
			{
				y1 = -1;
			}
			if (!int.TryParse(InLine.Substring(28, 4), out y2))
			{
				y2 = -1;
			}
			if (!int.TryParse(InLine.Substring(33, 4), out numOfObs))
			{
				numOfObs = -1;
			}
			if (numOfObs > 99)
			{
				numOfObs = 99;
			}
			if (!double.TryParse(InLine.Substring(38, 3), out pA1))
			{
				pA1 = -1.0;
			}
			pA1_Places = 0;
			if (!double.TryParse(InLine.Substring(42, 3), out pA2))
			{
				pA2 = -1.0;
			}
			pA2_Places = 0;
			sep1_LessThan = false;
			if (!double.TryParse(InLine.Substring(46, 5), out sep1))
			{
				sep1 = -1.0;
			}
			else
			{
				sep1_Places = Utilities.DecimalPlaces(InLine.Substring(46, 5));
			}
			sep2_LessThan = false;
			if (!double.TryParse(InLine.Substring(52, 5), out sep2))
			{
				sep2 = -1.0;
			}
			else
			{
				sep2_Places = Utilities.DecimalPlaces(InLine.Substring(52, 5));
			}
			if (!double.TryParse(InLine.Substring(58, 5), out mag1))
			{
				mag1 = -10.0;
			}
			else
			{
				mag1_Places = Utilities.DecimalPlaces(InLine.Substring(58, 5));
			}
			if (!double.TryParse(InLine.Substring(64, 5), out mag2))
			{
				mag2 = -10.0;
			}
			else
			{
				mag2_Places = Utilities.DecimalPlaces(InLine.Substring(64, 5));
			}
			spectrum = InLine.Substring(70, 9);
			if (!int.TryParse(InLine.Substring(80, 4), out pMRA))
			{
				pMRA = -999;
			}
			if (!int.TryParse(InLine.Substring(84, 4), out pMDec))
			{
				pMDec = -999;
			}
			bDSign = !(InLine.Substring(98, 1) == "-");
			if (!int.TryParse(InLine.Substring(99, 2), out bDZone))
			{
				bDZone = -99;
			}
			if (!int.TryParse(InLine.Substring(101, 5), out bDNumber))
			{
				bDNumber = -1;
			}
			wDSNote1 = InLine.Substring(107, 1);
			wDSNote2 = InLine.Substring(108, 1);
			wDSNote3 = InLine.Substring(109, 1);
			lineAdded = true;
			period = -1.0;
			axis = -1.0;
			inclination = -1.0;
			node = -1.0;
			t = -1.0;
			e = -1.0;
			periastron = -1.0;
		}

		public string SaveString()
		{
			return OutputString(FlagAddedLines: false);
		}

		public override string ToString()
		{
			return OutputString(FlagAddedLines: true);
		}

		private string OutputString(bool FlagAddedLines)
		{
			StringBuilder stringBuilder = new StringBuilder();
			if (xZPrimary > 0)
			{
				stringBuilder.Append(xZPrimary.ToString().PadLeft(6).PadRight(7));
			}
			else
			{
				stringBuilder.Append("".PadRight(7));
			}
			if (xZSecondary > 0)
			{
				stringBuilder.Append(xZSecondary.ToString().PadLeft(6).PadRight(7));
			}
			else
			{
				stringBuilder.Append("".PadRight(7));
			}
			if (xZPrinciple > 0)
			{
				stringBuilder.Append(xZPrinciple.ToString().PadLeft(6).PadRight(6));
			}
			else
			{
				stringBuilder.Append("".PadRight(6));
			}
			if (meanPosn)
			{
				stringBuilder.Append("M");
			}
			else
			{
				stringBuilder.Append(" ");
			}
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
			stringBuilder.Append(" " + iDCode.Trim().PadRight(3) + iDNumber.PadRight(4) + " " + iDPair.Trim().PadRight(5));
			if (y1 > 0)
			{
				stringBuilder.Append(y1.ToString().PadLeft(5));
			}
			else
			{
				stringBuilder.Append("".PadLeft(5));
			}
			if (y2 > 0)
			{
				stringBuilder.Append(y2.ToString().PadLeft(5));
			}
			else
			{
				stringBuilder.Append("".PadLeft(5));
			}
			if (numOfObs > 0)
			{
				stringBuilder.Append(numOfObs.ToString().PadLeft(3));
			}
			else
			{
				stringBuilder.Append("".PadLeft(3));
			}
			if (pA1 >= 0.0)
			{
				if (pA1_Places > 0)
				{
					stringBuilder.Append(Utilities.FormatNumber(pA1, 4, pA1_Places).PadRight(6).Substring(0, 6));
				}
				else
				{
					stringBuilder.Append(pA1.ToString().PadLeft(4).PadRight(6));
				}
			}
			else
			{
				stringBuilder.Append("".PadRight(6));
			}
			if (pA2 >= 0.0)
			{
				if (pA2_Places > 0)
				{
					stringBuilder.Append(Utilities.FormatNumber(pA2, 4, pA2_Places).PadRight(6).Substring(0, 6));
				}
				else
				{
					stringBuilder.Append(pA2.ToString().PadLeft(4).PadRight(6));
				}
			}
			else
			{
				stringBuilder.Append("".PadRight(6));
			}
			if (sep1 >= 0.0)
			{
				string text = Utilities.FormatNumber(sep1, 3, sep1_Places).PadRight(8);
				if (sep1_LessThan)
				{
					text = ("<" + text.TrimStart(new char[1] { ' ' })).PadLeft(8);
				}
				stringBuilder.Append(" " + text);
			}
			else
			{
				stringBuilder.Append("".PadRight(9));
			}
			if (sep2 >= 0.0)
			{
				string text = Utilities.FormatNumber(sep2, 3, sep2_Places).PadRight(8);
				if (sep2_LessThan)
				{
					text = ("<" + text.TrimStart(new char[1] { ' ' })).PadLeft(8);
				}
				stringBuilder.Append(text);
			}
			else
			{
				stringBuilder.Append("".PadRight(8));
			}
			if (mag1 >= 0.0)
			{
				stringBuilder.Append(Utilities.FormatNumber(mag1, 2, mag1_Places).PadRight(6));
			}
			else
			{
				stringBuilder.Append("".PadRight(6));
			}
			if (mag2 >= 0.0)
			{
				stringBuilder.Append(Utilities.FormatNumber(mag2, 2, mag2_Places).PadRight(6));
			}
			else
			{
				stringBuilder.Append("".PadRight(6));
			}
			stringBuilder.Append(spectrum.Trim().PadRight(10));
			if (pMRA > -999)
			{
				stringBuilder.Append(pMRA.ToString("+000;-000; 000"));
			}
			else
			{
				stringBuilder.Append("".PadRight(4));
			}
			if (pMDec > -999)
			{
				stringBuilder.Append(pMDec.ToString(" +000; -000;  000"));
			}
			else
			{
				stringBuilder.Append("".PadRight(5));
			}
			if (bDZone > -90)
			{
				if (!bDSign)
				{
					stringBuilder.Append(" -");
				}
				else
				{
					stringBuilder.Append(" +");
				}
				stringBuilder.Append(bDZone.ToString().PadLeft(2, '0'));
				stringBuilder.Append(bDNumber.ToString().PadLeft(5));
			}
			else
			{
				stringBuilder.Append("".PadRight(9));
			}
			stringBuilder.Append(" " + wDSNote1 + wDSNote2 + wDSNote3);
			if (LineAdded && FlagAddedLines)
			{
				stringBuilder.Append("#");
			}
			else
			{
				stringBuilder.Append(" ");
			}
			if (period > 0.0)
			{
				stringBuilder.Append(Utilities.FormatNumber(period, 4, period_Places).PadRight(11));
			}
			else
			{
				stringBuilder.Append("".PadRight(11));
			}
			if (axis > -0.1)
			{
				stringBuilder.Append(Utilities.FormatNumber(axis, 2, axis_Places).PadRight(9));
			}
			else
			{
				stringBuilder.Append("".PadRight(9));
			}
			if (inclination >= 0.0)
			{
				stringBuilder.Append(Utilities.FormatNumber(inclination, 4, inclination_Places).PadRight(9));
			}
			else
			{
				stringBuilder.Append("".PadRight(9));
			}
			if (node >= 0.0)
			{
				stringBuilder.Append(Utilities.FormatNumber(node, 4, node_Places).PadRight(9));
			}
			else
			{
				stringBuilder.Append("".PadRight(9));
			}
			if (t > 0.0)
			{
				stringBuilder.Append(Utilities.FormatNumber(t, 5, t_Places).PadRight(11));
			}
			else
			{
				stringBuilder.Append("".PadRight(11));
			}
			if (e >= 0.0)
			{
				stringBuilder.Append(Utilities.FormatNumber(e, 2, e_Places).PadRight(8));
			}
			else
			{
				stringBuilder.Append("".PadRight(8));
			}
			if (periastron >= 0.0)
			{
				stringBuilder.Append(Utilities.FormatNumber(periastron, 4, periastron_Places).PadRight(9));
			}
			else
			{
				stringBuilder.Append("".PadRight(9));
			}
			return stringBuilder.ToString();
		}

		public int CompareTo(object other)
		{
			if ((RA == ((XZDoubles)other).RA) & (Dec == ((XZDoubles)other).Dec) & (IDPair == ((XZDoubles)other).IDPair) & (IDCode == ((XZDoubles)other).IDCode))
			{
				return IDNumber.CompareTo(((XZDoubles)other).IDNumber);
			}
			if ((RA == ((XZDoubles)other).RA) & (Dec == ((XZDoubles)other).Dec) & (IDPair == ((XZDoubles)other).IDPair))
			{
				return IDCode.CompareTo(((XZDoubles)other).IDCode);
			}
			if ((RA == ((XZDoubles)other).RA) & (Dec == ((XZDoubles)other).Dec))
			{
				return IDPair.CompareTo(((XZDoubles)other).IDPair);
			}
			if (RA == ((XZDoubles)other).RA)
			{
				return ((XZDoubles)other).Dec.CompareTo(Dec);
			}
			return RA.CompareTo(((XZDoubles)other).RA);
		}
	}
}
