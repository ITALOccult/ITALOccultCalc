using System;
using System.Text;

namespace Occult
{
	public class AsteroidElements : IComparable
	{
		public static string[] Sources = new string[7] { "Astorb", "MPCorb", "AstDyS2", "Comet", "Orbfit", "JPL", "Gaia" };

		private string iDName = "";

		private string DiaSource = "";

		private string orbitSource = "";

		private string orbitDate = "";

		private string asteroidClass = "";

		private int iDNumber;

		private int epochYear;

		private int epochMonth;

		private int num_Moons;

		private int num_Rings;

		private double MeanAnomaly;

		private double epochDay;

		private double perihelion;

		private double node;

		private double i;

		private double e;

		private double a;

		private double Q;

		private double h0 = 25.0;

		private double g_phaseCoeff = 0.15;

		private double logRcoeff = 5.0;

		private double arc;

		private double obs;

		private double PEU;

		private double DiameterMean = 0.1;

		private double NumOpp;

		private double diameterUncertainty;

		private double errorMajor;

		private double errorMinor;

		private double errorPA;

		private const double Radian = 180.0 / Math.PI;

		public string IDName
		{
			get
			{
				return iDName;
			}
			set
			{
				iDName = value;
			}
		}

		public int IDNumber
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

		public double Meananomaly
		{
			get
			{
				return MeanAnomaly;
			}
			set
			{
				MeanAnomaly = value;
			}
		}

		public int EpochYear
		{
			get
			{
				return epochYear;
			}
			set
			{
				epochYear = value;
			}
		}

		public int EpochMonth
		{
			get
			{
				return epochMonth;
			}
			set
			{
				epochMonth = value;
			}
		}

		public double EpochDay
		{
			get
			{
				return epochDay;
			}
			set
			{
				epochDay = value;
			}
		}

		public double OsculatingJD => Utilities.JD_from_Date(EpochYear, EpochMonth, EpochDay);

		public double Perihelion
		{
			get
			{
				return perihelion;
			}
			set
			{
				perihelion = value;
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

		public double I
		{
			get
			{
				return i;
			}
			set
			{
				i = value;
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

		public double A
		{
			get
			{
				return a;
			}
			set
			{
				a = value;
			}
		}

		public double q
		{
			get
			{
				return Q;
			}
			set
			{
				Q = value;
			}
		}

		public double LogR_Coeff
		{
			get
			{
				return logRcoeff;
			}
			set
			{
				logRcoeff = value;
			}
		}

		public double H0
		{
			get
			{
				return h0;
			}
			set
			{
				h0 = value;
			}
		}

		public double G_phaseCoeff
		{
			get
			{
				return g_phaseCoeff;
			}
			set
			{
				g_phaseCoeff = value;
			}
		}

		public double Obs
		{
			get
			{
				return obs;
			}
			set
			{
				obs = value;
			}
		}

		public double PeakEphemUncert
		{
			get
			{
				return PEU;
			}
			set
			{
				PEU = value;
			}
		}

		public double Diameter_Mean
		{
			get
			{
				return DiameterMean;
			}
			set
			{
				DiameterMean = value;
			}
		}

		public double DiameterUncertainty
		{
			get
			{
				if (DiameterMean > 1.0)
				{
					return diameterUncertainty;
				}
				return DiameterMean / 10.0;
			}
			set
			{
				diameterUncertainty = value;
			}
		}

		public string Dia_Source
		{
			get
			{
				return DiaSource;
			}
			set
			{
				DiaSource = value;
			}
		}

		public double Arc
		{
			get
			{
				return arc;
			}
			set
			{
				arc = value;
			}
		}

		public double Num_Opp
		{
			get
			{
				return NumOpp;
			}
			set
			{
				NumOpp = value;
			}
		}

		public int Num_Rings
		{
			get
			{
				return num_Rings;
			}
			set
			{
				num_Rings = value;
			}
		}

		public int Num_Moons
		{
			get
			{
				return num_Moons;
			}
			set
			{
				num_Moons = value;
			}
		}

		public string OrbitSource
		{
			get
			{
				return orbitSource;
			}
			set
			{
				orbitSource = value;
			}
		}

		public string OrbitDate
		{
			get
			{
				return orbitDate;
			}
			set
			{
				orbitDate = value;
			}
		}

		public string AsteroidClass
		{
			get
			{
				return asteroidClass;
			}
			set
			{
				asteroidClass = value;
			}
		}

		public double ErrorMajor
		{
			get
			{
				return errorMajor;
			}
			set
			{
				errorMajor = value;
			}
		}

		public double ErrorMinor
		{
			get
			{
				return errorMinor;
			}
			set
			{
				errorMinor = value;
			}
		}

		public double ErrorPA
		{
			get
			{
				return errorPA;
			}
			set
			{
				errorPA = value;
			}
		}

		public int CompareTo(object other)
		{
			if ((IDNumber == 0) & (((AsteroidElements)other).IDNumber == 0))
			{
				return IDName.CompareTo(((AsteroidElements)other).IDName);
			}
			if ((IDNumber == 0) | (((AsteroidElements)other).IDNumber == 0))
			{
				return ((AsteroidElements)other).IDNumber.CompareTo(IDNumber);
			}
			return IDNumber.CompareTo(((AsteroidElements)other).IDNumber);
		}

		internal void ReadElementLine(string LineIn)
		{
			LineIn = LineIn.PadRight(143);
			if (!int.TryParse(LineIn.Substring(0, 7), out iDNumber))
			{
				iDNumber = 0;
			}
			IDName = LineIn.Substring(8, 16).Trim();
			if (!int.TryParse(LineIn.Substring(24, 5), out epochYear))
			{
				epochYear = 0;
			}
			if (!int.TryParse(LineIn.Substring(29, 2), out epochMonth))
			{
				epochMonth = 0;
			}
			if (!double.TryParse(LineIn.Substring(31, 8), out epochDay))
			{
				epochDay = 0.0;
			}
			if (!double.TryParse(LineIn.Substring(39, 11), out MeanAnomaly))
			{
				MeanAnomaly = 0.0;
			}
			if (!double.TryParse(LineIn.Substring(50, 11), out perihelion))
			{
				Perihelion = 0.0;
			}
			if (!double.TryParse(LineIn.Substring(61, 11), out node))
			{
				Node = 0.0;
			}
			if (!double.TryParse(LineIn.Substring(72, 11), out i))
			{
				i = 0.0;
			}
			if (!double.TryParse(LineIn.Substring(83, 11), out e))
			{
				e = 0.0;
			}
			if (!double.TryParse(LineIn.Substring(94, 13), out a))
			{
				a = 0.0;
			}
			Q = a * (1.0 - e);
			if (!double.TryParse(LineIn.Substring(107, 6), out h0))
			{
				h0 = 0.0;
			}
			if (!double.TryParse(LineIn.Substring(113, 6), out g_phaseCoeff))
			{
				g_phaseCoeff = 0.0;
			}
			logRcoeff = 5.0;
			if (!double.TryParse(LineIn.Substring(119, 7), out DiameterMean))
			{
				Diameter_Mean = 0.0;
			}
			DiaSource = LineIn.Substring(133, 2);
			if (!double.TryParse(LineIn.Substring(135, 6), out PEU))
			{
				PEU = 0.0;
			}
			if (!int.TryParse(LineIn.Substring(141, 1), out num_Rings))
			{
				num_Rings = 0;
			}
			if (!int.TryParse(LineIn.Substring(142, 1), out num_Moons))
			{
				num_Moons = 0;
			}
			double num2 = (ErrorPA = 0.0);
			double num5 = (ErrorMajor = (ErrorMinor = num2));
		}

		internal void ReadCSVElementLine(string LineIn)
		{
			string[] array = LineIn.Split(new char[1] { ',' });
			LineIn = LineIn.PadRight(143);
			if (!int.TryParse(array[0], out iDNumber))
			{
				iDNumber = 0;
			}
			IDName = array[1];
			if (!int.TryParse(array[2], out epochYear))
			{
				epochYear = 0;
			}
			if (!int.TryParse(array[3], out epochMonth))
			{
				epochMonth = 0;
			}
			if (!double.TryParse(array[4], out epochDay))
			{
				epochDay = 0.0;
			}
			if (!double.TryParse(array[5], out MeanAnomaly))
			{
				MeanAnomaly = 0.0;
			}
			if (!double.TryParse(array[6], out perihelion))
			{
				Perihelion = 0.0;
			}
			if (!double.TryParse(array[7], out node))
			{
				Node = 0.0;
			}
			if (!double.TryParse(array[8], out i))
			{
				i = 0.0;
			}
			if (!double.TryParse(array[9], out e))
			{
				e = 0.0;
			}
			if (!double.TryParse(array[10], out a))
			{
				a = 0.0;
			}
			Q = a * (1.0 - e);
			if (!double.TryParse(array[11], out h0))
			{
				h0 = 0.0;
			}
			if (!double.TryParse(array[12], out g_phaseCoeff))
			{
				g_phaseCoeff = 0.0;
			}
			logRcoeff = 5.0;
			if (!double.TryParse(array[13], out DiameterMean))
			{
				Diameter_Mean = 0.0;
			}
			DiaSource = array[14];
			if (!double.TryParse(array[15], out PEU))
			{
				PEU = 0.0;
			}
			if (!int.TryParse(array[16], out num_Rings))
			{
				num_Rings = 0;
			}
			if (!int.TryParse(array[17], out num_Moons))
			{
				num_Moons = 0;
			}
			if (array.Length > 18)
			{
				OrbitSource = array[18];
			}
			if (array.Length > 19)
			{
				OrbitDate = array[19];
			}
			if (array.Length > 20)
			{
				AsteroidClass = array[20];
			}
			if (array.Length > 21)
			{
				double.TryParse(array[21], out diameterUncertainty);
			}
			else
			{
				diameterUncertainty = DiameterMean / 10.0;
			}
			if (array.Length > 22)
			{
				double.TryParse(array[22], out errorMajor);
			}
			if (array.Length > 23)
			{
				double.TryParse(array[23], out errorMinor);
			}
			if (array.Length > 24)
			{
				double.TryParse(array[24], out errorPA);
			}
			if (array.Length > 25)
			{
				double.TryParse(array[25], out logRcoeff);
			}
			if (LogR_Coeff < 5.0)
			{
				LogR_Coeff = 5.0;
			}
		}

		internal bool ReadAstorbLine(string InLine)
		{
			if (InLine.Length < 224)
			{
				return false;
			}
			if (InLine.Contains("*"))
			{
				return false;
			}
			try
			{
				int.TryParse(InLine.Substring(0, 6), out iDNumber);
				IDName = InLine.Substring(7, 16);
				if (IDNumber == 134340)
				{
					IDName = "Pluto barycenter";
				}
				MeanAnomaly = double.Parse(InLine.Substring(115, 11));
				epochYear = int.Parse(InLine.Substring(106, 4));
				epochMonth = int.Parse(InLine.Substring(110, 2));
				epochDay = double.Parse(InLine.Substring(112, 3));
				perihelion = double.Parse(InLine.Substring(126, 11));
				node = double.Parse(InLine.Substring(137, 10));
				i = double.Parse(InLine.Substring(147, 10));
				e = double.Parse(InLine.Substring(158, 10));
				a = double.Parse(InLine.Substring(169, 13));
				Q = a * (1.0 - e);
				h0 = double.Parse(InLine.Substring(42, 6));
				g_phaseCoeff = double.Parse(InLine.Substring(48, 5));
				logRcoeff = 5.0;
				arc = double.Parse(InLine.Substring(94, 6));
				obs = double.Parse(InLine.Substring(100, 6));
				if (!double.TryParse(InLine.Substring(216, 8), out PEU))
				{
					PEU = 1.0;
				}
				orbitSource = Sources[0];
				NumOpp = 0.0;
				double num2 = (ErrorPA = 0.0);
				double num5 = (ErrorMajor = (ErrorMinor = num2));
				return true;
			}
			catch
			{
				return false;
			}
		}

		internal bool ReadGaiaLine(string InLine)
		{
			string[] array = InLine.Split(new char[1] { ',' });
			iDNumber = int.Parse(array[1]);
			IDName = array[2];
			if (IDNumber == 134340)
			{
				IDName = "Pluto barycenter";
			}
			h0 = double.Parse(array[3]);
			g_phaseCoeff = double.Parse(array[4]);
			logRcoeff = 5.0;
			epochYear = int.Parse(array[8].Substring(0, 4));
			epochMonth = int.Parse(array[8].Substring(4, 2));
			epochDay = double.Parse(array[8].Substring(6));
			MeanAnomaly = double.Parse(array[9]);
			perihelion = double.Parse(array[10]);
			node = double.Parse(array[11]);
			i = double.Parse(array[12]);
			e = double.Parse(array[13]);
			a = double.Parse(array[14]);
			Q = a * (1.0 - e);
			arc = double.Parse(array[6]);
			obs = double.Parse(array[7]);
			PEU = 0.01;
			orbitSource = Sources[6];
			NumOpp = 0.0;
			double num2 = (ErrorPA = 0.0);
			double num5 = (ErrorMajor = (ErrorMinor = num2));
			return true;
		}

		internal bool ReadMPCORBLine(string InLine)
		{
			int num = 0;
			try
			{
				int num2 = InLine.IndexOf("(", 165) + 1;
				num = InLine.IndexOf(")", 165) - num2;
				IDNumber = 0;
				if (num > 0)
				{
					int.TryParse(InLine.Substring(num2, num), out iDNumber);
				}
				IDName = InLine.Substring(174, 16);
				if (IDNumber == 134340)
				{
					IDName = "Pluto barycenter";
				}
				MeanAnomaly = double.Parse(InLine.Substring(26, 10));
				int num3 = 2000;
				if (InLine.Substring(20, 1) == "J")
				{
					num3 = 1900;
				}
				epochYear = int.Parse(InLine.Substring(21, 2)) + num3;
				epochMonth = " 123456789ABC".IndexOf(InLine.Substring(23, 1));
				epochDay = " 123456789ABCDEFGHIJKLMNOPQRSTUV".IndexOf(InLine.Substring(24, 1));
				perihelion = double.Parse(InLine.Substring(37, 10));
				node = double.Parse(InLine.Substring(48, 10));
				i = double.Parse(InLine.Substring(59, 10));
				e = double.Parse(InLine.Substring(70, 10));
				a = double.Parse(InLine.Substring(92, 11));
				Q = a * (1.0 - e);
				double.TryParse(InLine.Substring(8, 5), out h0);
				double.TryParse(InLine.Substring(14, 5), out g_phaseCoeff);
				logRcoeff = 5.0;
				NumOpp = double.Parse(InLine.Substring(122, 4));
				orbitSource = Sources[1];
				double num5 = (ErrorPA = 0.0);
				double num8 = (ErrorMajor = (ErrorMinor = num5));
				PEU = 0.0;
				return true;
			}
			catch
			{
				return false;
			}
		}

		internal void ReadAstDySLines(string InLine)
		{
			int num = InLine.IndexOf("'", 1);
			IDNumber = int.Parse(InLine.Substring(1, num - 1));
			Utilities.Date_from_JD(2400000.5 + double.Parse(InLine.Substring(14, 13)), out epochYear, out epochMonth, out epochDay);
			a = double.Parse(InLine.Substring(30, 22));
			e = double.Parse(InLine.Substring(55, 22));
			Q = a * (1.0 - e);
			i = double.Parse(InLine.Substring(80, 22));
			node = double.Parse(InLine.Substring(105, 22));
			perihelion = double.Parse(InLine.Substring(130, 22));
			MeanAnomaly = double.Parse(InLine.Substring(155, 22));
			double.TryParse(InLine.Substring(178, 5), out h0);
			double.TryParse(InLine.Substring(184, 5), out g_phaseCoeff);
			logRcoeff = 5.0;
			orbitSource = Sources[2];
			PEU = 0.0;
			double num3 = (ErrorPA = 0.0);
			double num6 = (ErrorMajor = (ErrorMinor = num3));
			NumOpp = 0.0;
		}

		internal void ReadIAALines(string InLine, string InLine2, string InLine3)
		{
			int.TryParse(InLine.Substring(0, 6), out iDNumber);
			IDName = InLine.Substring(7, 16);
			if (IDNumber == 134340)
			{
				IDName = "Pluto barycenter";
			}
			double.TryParse(InLine.Substring(26, 5), out h0);
			if (InLine.Substring(32, 6).IndexOf("X") > 0)
			{
				g_phaseCoeff = 0.15;
			}
			else
			{
				double.TryParse(InLine.Substring(32, 5), out g_phaseCoeff);
			}
			logRcoeff = 5.0;
			epochYear = int.Parse(InLine.Substring(41, 4));
			epochMonth = int.Parse(InLine.Substring(46, 2));
			epochDay = double.Parse(InLine.Substring(49, 2));
			MeanAnomaly = double.Parse(InLine2.Substring(7, 10));
			perihelion = double.Parse(InLine2.Substring(17, 10));
			node = double.Parse(InLine2.Substring(27, 10));
			i = double.Parse(InLine2.Substring(37, 10));
			e = double.Parse(InLine2.Substring(47, 10));
			a = double.Parse(InLine2.Substring(67));
			Q = a * (1.0 - e);
			NumOpp = double.Parse(InLine3.Substring(7, 3));
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			if (IDNumber > 0)
			{
				stringBuilder.AppendFormat("{0,7:F0}", IDNumber);
			}
			else
			{
				stringBuilder.AppendFormat("".PadRight(7));
			}
			stringBuilder.Append(string.Format(" {0,-16}", IDName).Substring(0, 17));
			stringBuilder.AppendFormat("{0,5}", epochYear);
			stringBuilder.AppendFormat("{0,2}", epochMonth);
			stringBuilder.AppendFormat("{0,10:F7}", epochDay);
			stringBuilder.AppendFormat("{0,11:F6}", MeanAnomaly);
			stringBuilder.AppendFormat("{0,11:F6}", perihelion);
			stringBuilder.AppendFormat("{0,11:F6}", node);
			stringBuilder.AppendFormat("{0,11:F6}", i);
			stringBuilder.AppendFormat("{0,11:F8}", e);
			stringBuilder.AppendFormat("{0,13:F8}", a);
			stringBuilder.AppendFormat("{0,6:F2}", h0);
			stringBuilder.AppendFormat("{0,6:F2}", g_phaseCoeff);
			stringBuilder.AppendFormat("{0,7:F1}", DiameterMean);
			stringBuilder.Append("".PadRight(7));
			stringBuilder.AppendFormat("{0,2}", DiaSource);
			if (PEU > 0.0)
			{
				stringBuilder.AppendFormat("{0,6:F2}", PEU);
			}
			else
			{
				stringBuilder.Append("".PadRight(6));
			}
			stringBuilder.AppendFormat("{0,1}", num_Rings);
			stringBuilder.AppendFormat("{0,1}", num_Moons);
			return stringBuilder.ToString();
		}

		public string UserEditString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			if (IDNumber > 0)
			{
				stringBuilder.AppendFormat("{0,7:F0}", IDNumber);
			}
			else
			{
				stringBuilder.AppendFormat("".PadRight(7));
			}
			stringBuilder.Append(string.Format(" {0,-16}", IDName).Substring(0, 17));
			stringBuilder.AppendFormat("{0,5}", epochYear);
			stringBuilder.AppendFormat("{0,2}", epochMonth);
			stringBuilder.AppendFormat("{0,10:F7}", epochDay);
			if (OrbitSource == Sources[0])
			{
				stringBuilder.AppendFormat("{0,11:F6}  ", MeanAnomaly);
				stringBuilder.AppendFormat("{0,11:F6}  ", perihelion);
				stringBuilder.AppendFormat("{0,11:F6}  ", node);
				stringBuilder.AppendFormat("{0,11:F6}  ", i);
				stringBuilder.AppendFormat("{0,11:F8}  ", e);
				stringBuilder.AppendFormat("{0,13:F8}  ", a);
			}
			else if (OrbitSource == Sources[1])
			{
				stringBuilder.AppendFormat("{0,10:F5}   ", MeanAnomaly);
				stringBuilder.AppendFormat("{0,10:F5}   ", perihelion);
				stringBuilder.AppendFormat("{0,10:F5}   ", node);
				stringBuilder.AppendFormat("{0,10:F5}   ", i);
				stringBuilder.AppendFormat("{0,10:F7}   ", e);
				stringBuilder.AppendFormat("{0,12:F7}   ", a);
			}
			else if (OrbitSource == Sources[2])
			{
				stringBuilder.AppendFormat("{0,13:F8}", MeanAnomaly);
				stringBuilder.AppendFormat("{0,13:F8}", perihelion);
				stringBuilder.AppendFormat("{0,13:F8}", node);
				stringBuilder.AppendFormat("{0,13:F8}", i);
				stringBuilder.AppendFormat("{0,13:F10}", e);
				stringBuilder.AppendFormat("{0,15:F10}", a);
			}
			else if (OrbitSource.Contains(Sources[3]))
			{
				stringBuilder.AppendFormat("{0,10:F5}   ", MeanAnomaly);
				stringBuilder.AppendFormat("{0,9:F4}    ", perihelion);
				stringBuilder.AppendFormat("{0,9:F4}    ", node);
				stringBuilder.AppendFormat("{0,9:F4}    ", i);
				stringBuilder.AppendFormat("{0,9:F6}    ", e);
				stringBuilder.AppendFormat("{0,11:F6}    ", a);
			}
			else
			{
				stringBuilder.AppendFormat("{0,11:F6}  ", MeanAnomaly);
				stringBuilder.AppendFormat("{0,11:F6}  ", perihelion);
				stringBuilder.AppendFormat("{0,11:F6}  ", node);
				stringBuilder.AppendFormat("{0,11:F6}  ", i);
				stringBuilder.AppendFormat("{0,11:F8}  ", e);
				stringBuilder.AppendFormat("{0,13:F8}  ", a);
			}
			stringBuilder.AppendFormat("{0,6:F}", h0);
			stringBuilder.AppendFormat("{0,6:F2}", g_phaseCoeff);
			stringBuilder.AppendFormat("{0,7:F1}", DiameterMean);
			if (DiaSource == "M")
			{
				stringBuilder.Append(string.Format("±{0,1:F1}", DiameterUncertainty).PadLeft(5));
			}
			else
			{
				stringBuilder.Append(string.Format("±{0,1:F1}", DiameterMean / 10.0).PadLeft(5));
			}
			stringBuilder.AppendFormat("{0,2}", DiaSource);
			if (PEU > 0.0)
			{
				stringBuilder.AppendFormat("{0,6:F3} ", PEU);
			}
			else
			{
				stringBuilder.Append("".PadRight(7));
			}
			stringBuilder.AppendFormat("{0,1} ", num_Rings);
			stringBuilder.AppendFormat("{0,1} ", num_Moons);
			stringBuilder.Append(OrbitSource.PadRight(10));
			stringBuilder.Append(OrbitDate.PadRight(10));
			stringBuilder.Append(AsteroidClass);
			return stringBuilder.ToString();
		}

		public string CSVString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			if (IDNumber > 0)
			{
				stringBuilder.AppendFormat("{0,1:F0},", IDNumber);
			}
			else
			{
				stringBuilder.AppendFormat(",");
			}
			stringBuilder.AppendFormat("{0,1},", IDName.Trim());
			stringBuilder.AppendFormat("{0,1},", epochYear);
			stringBuilder.AppendFormat("{0,1},", epochMonth);
			if (epochDay % 1.0 == 0.0)
			{
				stringBuilder.AppendFormat("{0,1:F0},", epochDay);
			}
			else
			{
				stringBuilder.AppendFormat("{0,1:F7},", epochDay);
			}
			if (OrbitSource == Sources[0])
			{
				stringBuilder.AppendFormat("{0,1:F6},", MeanAnomaly);
				stringBuilder.AppendFormat("{0,1:F6},", perihelion);
				stringBuilder.AppendFormat("{0,1:F6},", node);
				stringBuilder.AppendFormat("{0,1:F6},", i);
				stringBuilder.AppendFormat("{0,1:F8},", e);
				stringBuilder.AppendFormat("{0,1:F8},", a);
			}
			else if (OrbitSource == Sources[1])
			{
				stringBuilder.AppendFormat("{0,1:F5},", MeanAnomaly);
				stringBuilder.AppendFormat("{0,1:F5},", perihelion);
				stringBuilder.AppendFormat("{0,1:F5},", node);
				stringBuilder.AppendFormat("{0,1:F5},", i);
				stringBuilder.AppendFormat("{0,1:F7},", e);
				stringBuilder.AppendFormat("{0,1:F7},", a);
			}
			else if (OrbitSource == Sources[2])
			{
				stringBuilder.AppendFormat("{0,1:F8},", MeanAnomaly);
				stringBuilder.AppendFormat("{0,1:F8},", perihelion);
				stringBuilder.AppendFormat("{0,1:F8},", node);
				stringBuilder.AppendFormat("{0,1:F8},", i);
				stringBuilder.AppendFormat("{0,1:F10},", e);
				stringBuilder.AppendFormat("{0,1:F10},", a);
			}
			else
			{
				stringBuilder.AppendFormat("{0,1:F10},", MeanAnomaly);
				stringBuilder.AppendFormat("{0,1:F10},", perihelion);
				stringBuilder.AppendFormat("{0,1:F10},", node);
				stringBuilder.AppendFormat("{0,1:F10},", i);
				stringBuilder.AppendFormat("{0,1:F12},", e);
				stringBuilder.AppendFormat("{0,1:F12},", a);
			}
			stringBuilder.AppendFormat("{0,1:F2},", h0);
			stringBuilder.AppendFormat("{0,1:F2},", g_phaseCoeff);
			stringBuilder.AppendFormat("{0,1:F1},", DiameterMean);
			stringBuilder.Append(DiaSource.Trim() + ",");
			if (PEU > 0.0)
			{
				stringBuilder.AppendFormat("{0,1:F4},", PEU);
			}
			else
			{
				stringBuilder.Append(",");
			}
			stringBuilder.AppendFormat("{0,1},", num_Rings);
			stringBuilder.AppendFormat("{0,1},", num_Moons);
			stringBuilder.Append(OrbitSource + ",");
			stringBuilder.Append(OrbitDate + ",");
			stringBuilder.Append(AsteroidClass + ",");
			stringBuilder.AppendFormat("{0,1:F1},", DiameterUncertainty);
			stringBuilder.AppendFormat("{0,1:F5},", ErrorMajor);
			stringBuilder.AppendFormat("{0,1:F5},", ErrorMinor);
			stringBuilder.AppendFormat("{0,1:F1},", ErrorPA);
			stringBuilder.AppendFormat("{0,1:F1}", LogR_Coeff);
			return stringBuilder.ToString();
		}

		public static string CSVHeader()
		{
			return "Num,Name,year,month,day,MA,perih,node,i,e,a,H0,G,Dia,DiaSrc,PEU/POS_1sigma,#Rings,#Moons,OrbSrc,OrbDate,Class,DiaUncert,ErrorMajor,ErrorMinor,ErrorMajorPA,LogR_Coeff";
		}
	}
}
