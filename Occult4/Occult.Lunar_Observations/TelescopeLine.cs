using System;
using System.Text;

namespace Occult.Lunar_Observations
{
	internal class TelescopeLine : IComparable
	{
		private const double Radian = 180.0 / Math.PI;

		public static int SortField;

		public static bool UseOldFormat;

		private string telescopeCodeForEvent = "";

		private string telescopeType = "";

		private string mountType = "";

		private string driveType = "";

		private string eW = "E";

		private string nS = "N";

		private string datum = "";

		private string longSign = "+";

		private string latSign = "+";

		private string telescopeStation = "";

		private string telescopeTelescopeCode = "";

		private string verticalDatum = "M";

		private string telescopePlace = "";

		private double aperture;

		private double focalLength;

		private double longSec;

		private double latSec;

		private double altitude;

		private int longDeg;

		private int longMin;

		private int latDeg;

		private int latMin;

		private int longSec_DecPlaces;

		private int latSec_DecPlaces;

		private int alt_DecPlaces;

		private int datumNumber;

		public string TelescopeCodeForEvent
		{
			get
			{
				return telescopeCodeForEvent;
			}
			set
			{
				telescopeCodeForEvent = value;
			}
		}

		public string TelescopeType
		{
			get
			{
				return telescopeType;
			}
			set
			{
				telescopeType = value;
			}
		}

		public string MountType
		{
			get
			{
				return mountType;
			}
			set
			{
				mountType = value;
			}
		}

		public string DriveType
		{
			get
			{
				return driveType;
			}
			set
			{
				driveType = value;
			}
		}

		public double Aperture
		{
			get
			{
				return aperture;
			}
			set
			{
				aperture = value;
			}
		}

		public double FocalLength
		{
			get
			{
				return focalLength;
			}
			set
			{
				focalLength = value;
			}
		}

		public string LongSign
		{
			get
			{
				return longSign;
			}
			set
			{
				longSign = value;
			}
		}

		public string EW
		{
			get
			{
				return eW;
			}
			set
			{
				eW = value;
			}
		}

		public int LongDeg
		{
			get
			{
				return longDeg;
			}
			set
			{
				longDeg = value;
			}
		}

		public int LongMin
		{
			get
			{
				return longMin;
			}
			set
			{
				longMin = value;
			}
		}

		public double LongSec
		{
			get
			{
				return longSec;
			}
			set
			{
				longSec = value;
			}
		}

		public int LongSec_DecPlaces
		{
			get
			{
				return longSec_DecPlaces;
			}
			set
			{
				longSec_DecPlaces = value;
			}
		}

		public double LongitudeDeg => MakeLongitude();

		public double Longitude => MakeLongitude() / (180.0 / Math.PI);

		public string LatSign
		{
			get
			{
				return latSign;
			}
			set
			{
				latSign = value;
			}
		}

		public string NS
		{
			get
			{
				return nS;
			}
			set
			{
				nS = value;
			}
		}

		public int LatDeg
		{
			get
			{
				return latDeg;
			}
			set
			{
				latDeg = value;
			}
		}

		public int LatMin
		{
			get
			{
				return latMin;
			}
			set
			{
				latMin = value;
			}
		}

		public double LatSec
		{
			get
			{
				return latSec;
			}
			set
			{
				latSec = value;
			}
		}

		public int LatSec_DecPlaces
		{
			get
			{
				return latSec_DecPlaces;
			}
			set
			{
				latSec_DecPlaces = value;
			}
		}

		public double LatitudeDeg => MakeLatitude();

		public double Latitude => MakeLatitude() / (180.0 / Math.PI);

		public string Datum
		{
			get
			{
				return datum;
			}
			set
			{
				datum = value;
			}
		}

		public int DatumNumber
		{
			get
			{
				return datumNumber;
			}
			set
			{
				datumNumber = value;
			}
		}

		public double Altitude
		{
			get
			{
				return altitude;
			}
			set
			{
				altitude = value;
			}
		}

		public int Alt_DecPlaces
		{
			get
			{
				return alt_DecPlaces;
			}
			set
			{
				alt_DecPlaces = value;
			}
		}

		public string VerticalDatum
		{
			get
			{
				return verticalDatum;
			}
			set
			{
				verticalDatum = value;
			}
		}

		public string TelescopeStation
		{
			get
			{
				return telescopeStation;
			}
			set
			{
				telescopeStation = value;
			}
		}

		public string TelescopeTelescopeCode
		{
			get
			{
				return telescopeTelescopeCode;
			}
			set
			{
				telescopeTelescopeCode = value;
			}
		}

		public string TelescopePlace
		{
			get
			{
				return telescopePlace;
			}
			set
			{
				telescopePlace = value;
			}
		}

		private double MakeLongitude()
		{
			if (eW == "W")
			{
				return 0.0 - ((double)longDeg + (double)longMin / 60.0 + longSec / 3600.0);
			}
			return (double)longDeg + (double)longMin / 60.0 + longSec / 3600.0;
		}

		private double MakeLatitude()
		{
			if (nS == "S")
			{
				return 0.0 - ((double)latDeg + (double)latMin / 60.0 + latSec / 3600.0);
			}
			return (double)latDeg + (double)latMin / 60.0 + latSec / 3600.0;
		}

		public bool DecodeTelescopeLine(string TLine)
		{
			string text = TLine.PadRight(80);
			if (text.Substring(20, 20).Contains("E") | text.Substring(20, 20).Contains("W"))
			{
				if ("EW".Contains(text.Substring(32, 1)))
				{
					text = text.Insert(48, " ");
					text = text.Insert(5, " ");
					text = text.Insert(2, "  ");
				}
				else if ("EW".Contains(text.Substring(33, 1)))
				{
					text = text.Insert(48, " ");
					text = text.Insert(5, " ");
					text = text.Insert(2, " ");
				}
				else if ("EW".Contains(text.Substring(34, 1)))
				{
					text = text.Insert(56, " ");
					text = text.Insert(7, " ");
				}
				else if ("EW".Contains(text.Substring(36, 1)) & "NS".Contains(text.Substring(51, 1)))
				{
					text = text.Remove(7, 1) + " ";
				}
				else
				{
					char[] separator = new char[1] { ' ' };
					string[] array = text.Split(separator, StringSplitOptions.RemoveEmptyEntries);
					if (array.GetUpperBound(0) >= 13 && array[12].Length < 7)
					{
						text = (array[0] + array[1].PadLeft(5) + array[2].PadLeft(6) + array[3].PadLeft(7) + array[4].PadLeft(5) + array[5].PadLeft(3) + Fsec(array[6]) + array[7].PadLeft(2) + array[8].PadLeft(4) + array[9].PadLeft(3) + Fsec(array[10]) + array[11].PadLeft(2) + array[12].PadLeft(7) + array[13].PadLeft(6)).PadRight(80);
					}
				}
				telescopeCodeForEvent = text.Substring(1, 1);
				telescopeType = text.Substring(4, 1);
				mountType = text.Substring(5, 1);
				driveType = text.Substring(6, 1);
				if (!double.TryParse(text.Substring(8, 5), out aperture))
				{
					aperture = 0.0;
				}
				if (!double.TryParse(text.Substring(14, 6), out focalLength))
				{
					focalLength = 0.0;
				}
				if (!int.TryParse(text.Substring(22, 3), out longDeg))
				{
					longDeg = 0;
				}
				if (!int.TryParse(text.Substring(26, 2), out longMin))
				{
					longMin = 0;
				}
				if (!double.TryParse(text.Substring(29, 5), out longSec))
				{
					longSec = 0.0;
				}
				longSec_DecPlaces = Utilities.DecimalPlaces(text.Substring(29, 5));
				eW = text.Substring(35, 1).ToUpper();
				if (!"EW".Contains(eW))
				{
					eW = "E";
				}
				if (eW == "W")
				{
					longSign = "-";
				}
				if (!int.TryParse(text.Substring(38, 2), out latDeg))
				{
					latDeg = 0;
				}
				if (!int.TryParse(text.Substring(41, 2), out latMin))
				{
					latMin = 0;
				}
				if (!double.TryParse(text.Substring(44, 5), out latSec))
				{
					latSec = 0.0;
				}
				latSec_DecPlaces = Utilities.DecimalPlaces(text.Substring(44, 5));
				nS = text.Substring(50, 1).ToUpper();
				if (!"NS".Contains(nS))
				{
					nS = "N";
				}
				if (nS == "S")
				{
					latSign = "-";
				}
				if (!double.TryParse(text.Substring(52, 6), out altitude))
				{
					altitude = 0.0;
				}
				alt_DecPlaces = Utilities.DecimalPlaces(text.Substring(52, 6));
				datum = text.Substring(58, 12).Trim();
				datumNumber = GetDatumNumber();
				telescopeStation = text.Substring(70, 5);
				telescopeTelescopeCode = text.Substring(76, 4);
			}
			else
			{
				telescopeCodeForEvent = text.Substring(1, 1);
				telescopeType = text.Substring(4, 1);
				mountType = text.Substring(5, 1);
				driveType = text.Substring(6, 1);
				if (!double.TryParse(text.Substring(8, 4), out aperture))
				{
					aperture = 0.0;
				}
				if (!double.TryParse(text.Substring(14, 4), out focalLength))
				{
					focalLength = 0.0;
				}
				if (text.Substring(20, 1) == "-")
				{
					longSign = "-";
					eW = "W";
				}
				if (!int.TryParse(text.Substring(21, 3), out longDeg))
				{
					longDeg = 0;
				}
				if (!int.TryParse(text.Substring(24, 2), out longMin))
				{
					longMin = 0;
				}
				if (!double.TryParse(text.Substring(26, 5), out longSec))
				{
					longSec = 0.0;
				}
				longSec_DecPlaces = Utilities.DecimalPlaces(text.Substring(26, 5));
				if (text.Substring(32, 1) == "-")
				{
					latSign = "-";
					nS = "S";
				}
				if (!int.TryParse(text.Substring(33, 2), out latDeg))
				{
					latDeg = 0;
				}
				if (!int.TryParse(text.Substring(35, 2), out latMin))
				{
					latMin = 0;
				}
				if (!double.TryParse(text.Substring(37, 5), out latSec))
				{
					latSec = 0.0;
				}
				latSec_DecPlaces = Utilities.DecimalPlaces(text.Substring(37, 5));
				if (!int.TryParse(text.Substring(43, 2), out datumNumber))
				{
					datumNumber = 0;
				}
				datum = GetOldDatumID();
				if (!double.TryParse(text.Substring(46, 6), out altitude))
				{
					altitude = 0.0;
				}
				alt_DecPlaces = Utilities.DecimalPlaces(text.Substring(46, 6));
				if (text.Substring(52, 1) == "E")
				{
					verticalDatum = "E";
				}
			}
			return true;
		}

		private string Fsec(string arg)
		{
			if (!arg.Contains("."))
			{
				arg += ".";
			}
			int num = 3 - arg.IndexOf(".");
			if (num < 0)
			{
				num = 0;
			}
			return ("".PadRight(num) + arg).PadRight(6);
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append("T");
			stringBuilder.Append(telescopeCodeForEvent.PadRight(1).Substring(0, 1) + "  ");
			stringBuilder.Append(telescopeType.PadRight(1).Substring(0, 1));
			stringBuilder.Append(mountType.PadRight(1).Substring(0, 1));
			stringBuilder.Append(driveType.PadRight(1).Substring(0, 1));
			if (UseOldFormat)
			{
				stringBuilder.AppendFormat("{0,6:F1}", aperture);
				stringBuilder.AppendFormat("{0,7:F1}", focalLength);
				stringBuilder.AppendFormat("{0,5:F0}", longDeg);
				stringBuilder.AppendFormat("{0,3:F0}", longMin);
				if (longSec_DecPlaces == 0)
				{
					stringBuilder.AppendFormat(" {0,2:F0}.  ", longSec);
				}
				else if (longSec_DecPlaces == 1)
				{
					stringBuilder.AppendFormat(" {0,4:F1} ", longSec);
				}
				else
				{
					stringBuilder.AppendFormat(" {0,5:F2}", longSec);
				}
				stringBuilder.Append(" " + EW);
				stringBuilder.AppendFormat("{0,4:F0}", latDeg);
				stringBuilder.AppendFormat("{0,3:F0}", latMin);
				if (latSec_DecPlaces == 0)
				{
					stringBuilder.AppendFormat(" {0,2:F0}.  ", latSec);
				}
				else if (latSec_DecPlaces == 1)
				{
					stringBuilder.AppendFormat(" {0,4:F1} ", latSec);
				}
				else
				{
					stringBuilder.AppendFormat(" {0,5:F2}", latSec);
				}
				stringBuilder.Append(" " + NS);
				stringBuilder.AppendFormat("{0,7:F" + alt_DecPlaces + "}", altitude);
				stringBuilder.Append(datum.PadRight(12));
				stringBuilder.Append(telescopeStation.PadRight(5).Substring(0, 5));
				stringBuilder.Append(" " + telescopeTelescopeCode.PadRight(4).Substring(0, 4));
			}
			else
			{
				stringBuilder.AppendFormat(" {0,4:F0}  ", aperture);
				stringBuilder.AppendFormat("{0,4:F0}  ", focalLength);
				stringBuilder.Append(longSign);
				stringBuilder.AppendFormat("{0,3:F0}", longDeg);
				stringBuilder.AppendFormat("{0,2:F0}", longMin);
				if (longSec_DecPlaces == 0)
				{
					stringBuilder.AppendFormat("{0,2:F0}.  ", longSec);
				}
				else if (longSec_DecPlaces == 1)
				{
					stringBuilder.AppendFormat("{0,4:F1} ", longSec);
				}
				else
				{
					stringBuilder.AppendFormat("{0,5:F2}", longSec);
				}
				stringBuilder.Append(" " + latSign);
				stringBuilder.AppendFormat("{0,2:F0}", latDeg);
				stringBuilder.AppendFormat("{0,2:F0}", latMin);
				if (latSec_DecPlaces == 0)
				{
					stringBuilder.AppendFormat("{0,2:F0}.  ", latSec);
				}
				else if (latSec_DecPlaces == 1)
				{
					stringBuilder.AppendFormat("{0,4:F1} ", latSec);
				}
				else
				{
					stringBuilder.AppendFormat("{0,5:F2}", latSec);
				}
				stringBuilder.AppendFormat(" {0,2:F0} ", datumNumber);
				if (alt_DecPlaces == 0)
				{
					stringBuilder.AppendFormat("{0,4:F0}  ", altitude);
				}
				else
				{
					stringBuilder.AppendFormat("{0,6:F1}", altitude);
				}
				stringBuilder.Append(verticalDatum);
			}
			return stringBuilder.ToString();
		}

		public string ToShortString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(telescopeCodeForEvent.PadRight(1).Substring(0, 1) + "  ");
			stringBuilder.AppendFormat("{0,3:F0}    ", aperture);
			stringBuilder.Append(longSign);
			stringBuilder.AppendFormat("{0,3:F0}", longDeg);
			stringBuilder.AppendFormat("{0,3:F0}", longMin);
			if (longSec_DecPlaces == 0)
			{
				stringBuilder.AppendFormat("{0,3:F0}.  ", longSec);
			}
			else if (longSec_DecPlaces == 1)
			{
				stringBuilder.AppendFormat("{0,5:F1} ", longSec);
			}
			else
			{
				stringBuilder.AppendFormat("{0,6:F2}", longSec);
			}
			stringBuilder.Append("  " + latSign);
			stringBuilder.AppendFormat("{0,2:F0}", latDeg);
			stringBuilder.AppendFormat("{0,3:F0}", latMin);
			if (latSec_DecPlaces == 0)
			{
				stringBuilder.AppendFormat("{0,3:F0}.  ", latSec);
			}
			else if (latSec_DecPlaces == 1)
			{
				stringBuilder.AppendFormat("{0,5:F1} ", latSec);
			}
			else
			{
				stringBuilder.AppendFormat("{0,6:F2}", latSec);
			}
			stringBuilder.AppendFormat("{0,4:F0}", altitude);
			return stringBuilder.ToString();
		}

		public int CompareTo(object other)
		{
			return SortField switch
			{
				0 => TelescopeCodeForEvent.CompareTo(((TelescopeLine)other).TelescopeCodeForEvent), 
				1 => LongitudeDeg.CompareTo(((TelescopeLine)other).LongitudeDeg), 
				2 => LatitudeDeg.CompareTo(((TelescopeLine)other).LatitudeDeg), 
				_ => 0, 
			};
		}

		internal int GetDatumNumber()
		{
			if (datum == "")
			{
				return 0;
			}
			if (datum == "ARGENT")
			{
				return 64;
			}
			if (datum == "AU1966")
			{
				return 66;
			}
			if (datum == "BRAZIL")
			{
				return 13;
			}
			if (datum == "CAPE")
			{
				return 63;
			}
			if (datum == "CAPECAN")
			{
				return 30;
			}
			if (datum == "EP1950")
			{
				return 16;
			}
			if (datum == "FIJI")
			{
				return 48;
			}
			if (datum == "GB1936")
			{
				return 34;
			}
			if (datum == "GBSN80")
			{
				return 16;
			}
			if (datum == "GPS")
			{
				return 84;
			}
			if (datum == "GPS (WGS84)")
			{
				return 84;
			}
			if (datum == "GPS(WGS84)")
			{
				return 84;
			}
			if (datum == "HERMANN")
			{
				return 73;
			}
			if (datum == "INDIAN")
			{
				return 22;
			}
			if (datum == "LUZON")
			{
				return 26;
			}
			if (datum == "NAD1927")
			{
				return 29;
			}
			if (datum == "NL1885")
			{
				return 81;
			}
			if (datum == "NZ1949")
			{
				return 28;
			}
			if (datum == "POTSDAM")
			{
				return 37;
			}
			if (datum == "OHAWAII")
			{
				return 33;
			}
			if (datum == "PULKOVO")
			{
				return 40;
			}
			if (datum == "SAM1969")
			{
				return 41;
			}
			if (datum == "TOKYO")
			{
				return 46;
			}
			if (datum == "WGS84")
			{
				return 84;
			}
			if (datum == "WHITESA")
			{
				return 31;
			}
			if (datum == "AMER1855")
			{
				return 81;
			}
			if (datum == "ARCCAPE")
			{
				return 63;
			}
			if (datum == "CAMPO-INCHAU")
			{
				return 64;
			}
			if (datum == "CAPECANAWER")
			{
				return 30;
			}
			if (datum == "ED")
			{
				return 16;
			}
			if (datum == "ED1950")
			{
				return 16;
			}
			if (datum == "ED 1950")
			{
				return 16;
			}
			if (datum == "ETRS89")
			{
				return 80;
			}
			if (datum == "GDA94")
			{
				return 86;
			}
			if (datum == "HERMANNSKOG")
			{
				return 73;
			}
			if (datum == "INDIAN EVERE")
			{
				return 22;
			}
			if (datum == "JGD2000")
			{
				return 85;
			}
			if (datum == "NAD27")
			{
				return 29;
			}
			if (datum == "NAD 1927")
			{
				return 29;
			}
			if (datum == "NAD1983")
			{
				return 82;
			}
			if (datum == "NAD83")
			{
				return 82;
			}
			if (datum == "NZGD2000")
			{
				return 87;
			}
			if (datum == "ORDN1936")
			{
				return 34;
			}
			if (datum == "PROV1956")
			{
				return 38;
			}
			if (datum == "TD")
			{
				return 46;
			}
			if (datum == "S42 PULKOVO")
			{
				return 40;
			}
			if (datum == "WGS 1984")
			{
				return 84;
			}
			return 0;
		}

		private string GetOldDatumID()
		{
			if (datumNumber == 0)
			{
				return "";
			}
			if (datumNumber == 82)
			{
				return "NAD83";
			}
			if (datumNumber == 84)
			{
				return "WGS84";
			}
			if (datumNumber > 84)
			{
				return "WGS84";
			}
			if (datumNumber == 66)
			{
				return "AU1966";
			}
			if (datumNumber == 16)
			{
				return "EP1950";
			}
			if (datumNumber == 29)
			{
				return "NAD1927";
			}
			if (datumNumber == 3)
			{
				return "WHITESA";
			}
			if (datumNumber == 4)
			{
				return "GBSN80";
			}
			if (datumNumber == 41)
			{
				return "SAM1969";
			}
			if (datumNumber == 46)
			{
				return "TOKYO";
			}
			if (datumNumber == 28)
			{
				return "NZ1949";
			}
			if (datumNumber == 63)
			{
				return "CAPE";
			}
			if (datumNumber == 40)
			{
				return "PULKOVO";
			}
			if (datumNumber == 22)
			{
				return "INDIAN";
			}
			if (datumNumber == 33)
			{
				return "OHAWAII";
			}
			if (datumNumber == 34)
			{
				return "BRAZIL";
			}
			if (datumNumber == 13)
			{
				return "LUZON";
			}
			if (datumNumber == 34)
			{
				return "GB1936";
			}
			if (datumNumber == 37)
			{
				return "POTSDAM";
			}
			if (datumNumber == 17)
			{
				return "ARGENT";
			}
			if (datumNumber == 73)
			{
				return "HERMANN";
			}
			if (datumNumber == 18)
			{
				return "NL1885";
			}
			if (datumNumber == 30)
			{
				return "CAPECAN";
			}
			if (datumNumber == 20)
			{
				return "FIJI";
			}
			return "";
		}
	}
}
