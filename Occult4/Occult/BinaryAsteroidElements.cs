using System;
using System.Text;

namespace Occult
{
	internal class BinaryAsteroidElements : IComparable
	{
		private string IDmoonName;

		private string IDidentifier;

		private int year;

		private int month;

		private int referenceFrame;

		private double IDnumber;

		private double meanAnomaly;

		private double day;

		private double perihelion;

		private double node;

		private double i;

		private double e;

		private double a;

		private double diameter = 1.0;

		private double periodDays;

		private double epoch;

		public string BinaryHeader = "Number,Name,Satellite name,Dia(km),a,Period(days),e,i,epoch,MA,peri,node,RefFrame 1=Eclip 2=Equat";

		public static string BinaryHeaderFormatted = " Number  Name             Satellite name        Dia(km) a           Period(dy) e          i          epoch       MA         peri       node       RefFrame 1=Eclip 2=Equat";

		public double IDAsteroidNumber
		{
			get
			{
				return IDnumber;
			}
			set
			{
				IDnumber = value;
			}
		}

		public string IDAsteroidIdentifier
		{
			get
			{
				return IDidentifier;
			}
			set
			{
				IDidentifier = value;
			}
		}

		public string IDMoonName
		{
			get
			{
				return IDmoonName;
			}
			set
			{
				IDmoonName = value;
			}
		}

		public int Year
		{
			get
			{
				return year;
			}
			set
			{
				year = value;
			}
		}

		public int Month
		{
			get
			{
				return month;
			}
			set
			{
				month = value;
			}
		}

		public double Day
		{
			get
			{
				return day;
			}
			set
			{
				day = value;
			}
		}

		public double Epoch
		{
			get
			{
				return epoch;
			}
			set
			{
				epoch = value;
			}
		}

		public double PeriodDays
		{
			get
			{
				return periodDays;
			}
			set
			{
				periodDays = value;
			}
		}

		public double Meananomaly
		{
			get
			{
				return meanAnomaly;
			}
			set
			{
				meanAnomaly = value;
			}
		}

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

		public double Diameter
		{
			get
			{
				return diameter;
			}
			set
			{
				diameter = value;
			}
		}

		public int ReferenceFrame
		{
			get
			{
				return referenceFrame;
			}
			set
			{
				referenceFrame = value;
			}
		}

		public string IDAsteroidIdentifier_SearchOrdered()
		{
			if (IDAsteroidIdentifier.Length < 7)
			{
				return IDAsteroidIdentifier;
			}
			return IDAsteroidIdentifier.Substring(0, 6) + IDAsteroidIdentifier.Substring(7).Trim().PadLeft(3, '0') + IDAsteroidIdentifier.Substring(6, 1);
		}

		public int CompareTo(object other)
		{
			if ((IDAsteroidNumber == 0.0) & (((BinaryAsteroidElements)other).IDAsteroidNumber == 0.0))
			{
				return IDAsteroidIdentifier.CompareTo(((BinaryAsteroidElements)other).IDAsteroidIdentifier);
			}
			if ((IDAsteroidNumber == 0.0) | (((BinaryAsteroidElements)other).IDAsteroidNumber == 0.0))
			{
				return ((BinaryAsteroidElements)other).IDAsteroidNumber.CompareTo(IDAsteroidNumber);
			}
			return IDAsteroidNumber.CompareTo(((BinaryAsteroidElements)other).IDAsteroidNumber);
		}

		internal void ReadElementLine(string LineIn)
		{
			if (!double.TryParse(LineIn.Substring(0, 7), out IDnumber))
			{
				IDnumber = 0.0;
			}
			IDAsteroidIdentifier = LineIn.Substring(8, 16).Trim();
			IDmoonName = LineIn.Substring(25, 21).Trim();
			if (!double.TryParse(LineIn.Substring(46, 7), out diameter))
			{
				diameter = 0.0;
			}
			if (!double.TryParse(LineIn.Substring(53, 13), out a))
			{
				a = 0.0;
			}
			if (!double.TryParse(LineIn.Substring(66, 11), out periodDays))
			{
				periodDays = 1.0;
			}
			if (!double.TryParse(LineIn.Substring(77, 11), out e))
			{
				e = 0.0;
			}
			if (!double.TryParse(LineIn.Substring(88, 11), out i))
			{
				i = 0.0;
			}
			if (!int.TryParse(LineIn.Substring(99, 5), out year))
			{
				year = 0;
			}
			if (!int.TryParse(LineIn.Substring(104, 2), out month))
			{
				month = 0;
			}
			if (!double.TryParse(LineIn.Substring(106, 8), out day))
			{
				day = 0.0;
			}
			if (!double.TryParse(LineIn.Substring(114, 11), out meanAnomaly))
			{
				meanAnomaly = 0.0;
			}
			if (!double.TryParse(LineIn.Substring(125, 11), out perihelion))
			{
				perihelion = 0.0;
			}
			if (!double.TryParse(LineIn.Substring(136, 11), out node))
			{
				node = 0.0;
			}
			if (!int.TryParse(LineIn.Substring(147, 2), out referenceFrame))
			{
				referenceFrame = 0;
			}
		}

		internal void ReadCSVElementLine(string LineIn)
		{
			string[] array = LineIn.Split(new char[1] { ',' });
			if (!double.TryParse(array[0], out IDnumber))
			{
				IDnumber = 0.0;
			}
			IDAsteroidIdentifier = array[1];
			IDMoonName = array[2];
			if (!double.TryParse(array[3], out diameter))
			{
				diameter = 0.0;
			}
			if (!double.TryParse(array[4], out a))
			{
				a = 0.0;
			}
			if (!double.TryParse(array[5], out periodDays))
			{
				periodDays = 0.0;
			}
			if (!double.TryParse(array[6], out e))
			{
				e = 0.0;
			}
			if (!double.TryParse(array[7], out i))
			{
				i = 0.0;
			}
			if (!double.TryParse(array[8], out epoch))
			{
				epoch = 0.0;
			}
			if (epoch > 0.0)
			{
				Utilities.Date_from_JD(epoch, out year, out month, out day);
			}
			else
			{
				day = (year = (month = 0));
			}
			if (!double.TryParse(array[9], out meanAnomaly))
			{
				meanAnomaly = 0.0;
			}
			if (!double.TryParse(array[10], out perihelion))
			{
				perihelion = 0.0;
			}
			if (!double.TryParse(array[11], out node))
			{
				node = 0.0;
			}
			if (array.Length > 12)
			{
				if (!int.TryParse(array[12], out referenceFrame))
				{
					referenceFrame = 0;
				}
			}
			else
			{
				referenceFrame = 0;
			}
		}

		public string ToCSVString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			if (IDnumber > 0.0)
			{
				stringBuilder.Append(IDnumber + ",");
			}
			else
			{
				stringBuilder.Append(",");
			}
			stringBuilder.Append(IDidentifier.Trim());
			stringBuilder.Append("," + IDmoonName.Trim());
			if (diameter != 0.0)
			{
				if (diameter % 1.0 != 0.0)
				{
					stringBuilder.AppendFormat(",{0,1:F1},", diameter);
				}
				else
				{
					stringBuilder.AppendFormat(",{0,1:f0},", diameter);
				}
			}
			else
			{
				stringBuilder.Append(",,");
			}
			if (a != 0.0)
			{
				if (a % 1.0 != 0.0)
				{
					stringBuilder.AppendFormat("{0,1:F3},", a);
				}
				else
				{
					stringBuilder.AppendFormat("{0,1:f0},", a);
				}
			}
			else
			{
				stringBuilder.Append(",");
			}
			if (periodDays != 0.0)
			{
				stringBuilder.AppendFormat("{0,1:F6},", periodDays);
			}
			else
			{
				stringBuilder.Append(",");
			}
			if (e != 0.0)
			{
				stringBuilder.AppendFormat("{0,1:F4},", e);
			}
			else
			{
				stringBuilder.Append(",");
			}
			if (i != 0.0)
			{
				stringBuilder.AppendFormat("{0,1:F2},", i);
			}
			else
			{
				stringBuilder.Append(",");
			}
			if (year > 1999)
			{
				stringBuilder.AppendFormat("{0,1:f2},", Utilities.JD_from_Date(year, month, day));
			}
			else
			{
				stringBuilder.Append(",");
			}
			if (meanAnomaly != 0.0)
			{
				stringBuilder.AppendFormat("{0,1:F3},", meanAnomaly);
			}
			else
			{
				stringBuilder.Append(",");
			}
			if (perihelion != 0.0)
			{
				stringBuilder.AppendFormat("{0,1:F2},", perihelion);
			}
			else
			{
				stringBuilder.Append(",");
			}
			if (node != 0.0)
			{
				stringBuilder.AppendFormat("{0,1:F2},", node);
			}
			else
			{
				stringBuilder.Append(",");
			}
			stringBuilder.AppendFormat("{0,2:F0}", referenceFrame);
			return stringBuilder.ToString();
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			if (IDnumber > 0.0)
			{
				stringBuilder.AppendFormat("{0,7:F0}", IDnumber);
			}
			else
			{
				stringBuilder.AppendFormat("".PadRight(7));
			}
			stringBuilder.Append(" " + IDidentifier.PadRight(16).Substring(0, 16));
			stringBuilder.AppendFormat(" " + IDmoonName.PadRight(21).Substring(0, 21));
			if (diameter != 0.0)
			{
				if (diameter % 1.0 != 0.0)
				{
					stringBuilder.AppendFormat("{0,7:F1}", diameter);
				}
				else
				{
					stringBuilder.AppendFormat("{0,5}. ", diameter);
				}
			}
			else
			{
				stringBuilder.Append("    0  ");
			}
			if (a != 0.0)
			{
				if (a % 1.0 != 0.0)
				{
					stringBuilder.AppendFormat("{0,13:F3}", a);
				}
				else
				{
					stringBuilder.AppendFormat("{0,9}.   ", a);
				}
			}
			else
			{
				stringBuilder.Append("        0    ");
			}
			if (periodDays != 0.0)
			{
				stringBuilder.AppendFormat("{0,11:F6}", periodDays);
			}
			else
			{
				stringBuilder.Append("   0       ");
			}
			if (e != 0.0)
			{
				stringBuilder.AppendFormat("{0,11:F8}", e);
			}
			else
			{
				stringBuilder.Append(" 0         ");
			}
			if (i != 0.0)
			{
				stringBuilder.AppendFormat("{0,11:F6}", i);
			}
			else
			{
				stringBuilder.Append("   0       ");
			}
			stringBuilder.AppendFormat("{0,5}", year);
			stringBuilder.AppendFormat("{0,2}", month);
			stringBuilder.AppendFormat("{0,8:F5}", day);
			if (meanAnomaly != 0.0)
			{
				stringBuilder.AppendFormat("{0,11:F6}", meanAnomaly);
			}
			else
			{
				stringBuilder.Append("   0       ");
			}
			if (perihelion != 0.0)
			{
				stringBuilder.AppendFormat("{0,11:F6}", perihelion);
			}
			else
			{
				stringBuilder.Append("   0       ");
			}
			if (node != 0.0)
			{
				stringBuilder.AppendFormat("{0,11:F6}", node);
			}
			else
			{
				stringBuilder.Append("   0       ");
			}
			stringBuilder.AppendFormat("{0,2:F0}", referenceFrame);
			return stringBuilder.ToString();
		}

		public string MultiLineElements()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append("Number and ID : ".PadLeft(18));
			if (IDnumber > 0.0)
			{
				stringBuilder.AppendFormat("{0,1:F0},", IDnumber);
			}
			else
			{
				stringBuilder.AppendFormat(",");
			}
			stringBuilder.Append(" " + IDidentifier + "\r\n");
			stringBuilder.AppendFormat("Satellite name : ".PadLeft(18) + IDmoonName + "\r\n" + "Dia (km) : ".PadLeft(18));
			if (diameter != 0.0)
			{
				if (diameter % 1.0 != 0.0)
				{
					stringBuilder.AppendFormat("{0,1:F1}", diameter);
				}
				else
				{
					stringBuilder.AppendFormat("{0,1}.", diameter);
				}
			}
			stringBuilder.Append("\r\n" + "a (km) : ".PadLeft(18));
			if (a != 0.0)
			{
				if (a % 1.0 != 0.0)
				{
					stringBuilder.AppendFormat("{0,1:F3}", a);
				}
				else
				{
					stringBuilder.AppendFormat("{0,1}.", a);
				}
			}
			stringBuilder.Append("\r\n" + "Period (days) : ".PadLeft(18));
			if (periodDays != 0.0)
			{
				stringBuilder.AppendFormat("{0,1:F6}", periodDays);
			}
			stringBuilder.Append("\r\n" + "e : ".PadLeft(18));
			if (e != 0.0)
			{
				stringBuilder.AppendFormat("{0,11:F8}", e);
			}
			stringBuilder.Append("\r\n" + "i : ".PadLeft(18));
			if (i != 0.0)
			{
				stringBuilder.AppendFormat("{0,1:F6}", i);
			}
			stringBuilder.Append("\r\n" + "Epoch : ".PadLeft(18));
			if (Year > 1900)
			{
				stringBuilder.AppendFormat("{0,4} ", year);
				stringBuilder.Append(Utilities.ShortMonths[month]);
				stringBuilder.AppendFormat(" {0,1:F5}", day);
			}
			stringBuilder.Append("\r\n" + "Mean Anomaly : ".PadLeft(18));
			if (meanAnomaly != 0.0)
			{
				stringBuilder.AppendFormat("{0,1:F6}", meanAnomaly);
			}
			stringBuilder.Append("\r\n" + "perihelion : ".PadLeft(18));
			if (perihelion != 0.0)
			{
				stringBuilder.AppendFormat("{0,1:F6}", perihelion);
			}
			stringBuilder.Append("\r\n" + "Node : ".PadLeft(18));
			if (node != 0.0)
			{
				stringBuilder.AppendFormat("{0,1:F6}", node);
			}
			stringBuilder.Append("\r\n" + "Reference frame : ".PadLeft(18));
			if (referenceFrame == 1)
			{
				stringBuilder.Append("Ecliptic");
			}
			else
			{
				stringBuilder.Append("Equator");
			}
			stringBuilder.Append("\r\n");
			return stringBuilder.ToString();
		}
	}
}
