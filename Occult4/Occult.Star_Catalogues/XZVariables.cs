using System;
using System.Text;

namespace Occult.Star_Catalogues
{
	internal class XZVariables : IComparable
	{
		private int xz;

		private int maxMagPlaces;

		private int minMagPlaces;

		private int epochPlaces;

		private int periodPlaces;

		private double maxMag = -20.0;

		private double minMag = -20.0;

		private double epoch;

		private double period;

		private string name;

		private string vType;

		private string minMagFlag;

		private string photoSystem = "  ";

		public int XZ
		{
			get
			{
				return xz;
			}
			set
			{
				xz = value;
			}
		}

		public string Name
		{
			get
			{
				return name;
			}
			set
			{
				name = value;
			}
		}

		public string VType
		{
			get
			{
				return vType;
			}
			set
			{
				vType = value;
			}
		}

		public double MaxMag
		{
			get
			{
				return maxMag;
			}
			set
			{
				maxMag = value;
			}
		}

		public int MaxMagPlaces
		{
			get
			{
				return maxMagPlaces;
			}
			set
			{
				maxMagPlaces = value;
			}
		}

		public double MinMag
		{
			get
			{
				return minMag;
			}
			set
			{
				minMag = value;
			}
		}

		public int MinMagPlaces
		{
			get
			{
				return minMagPlaces;
			}
			set
			{
				minMagPlaces = value;
			}
		}

		public string MinMagFlag
		{
			get
			{
				return minMagFlag;
			}
			set
			{
				minMagFlag = value;
			}
		}

		public string PhotoSystem
		{
			get
			{
				return photoSystem;
			}
			set
			{
				photoSystem = value;
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

		public int EpochPlaces
		{
			get
			{
				return epochPlaces;
			}
			set
			{
				epochPlaces = value;
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

		public int PeriodPlaces
		{
			get
			{
				return periodPlaces;
			}
			set
			{
				periodPlaces = value;
			}
		}

		internal void DecodeLine(string InLine)
		{
			if (!int.TryParse(InLine.Substring(0, 6), out xz))
			{
				xz = 0;
			}
			name = InLine.Substring(7, 30);
			vType = InLine.Substring(37, 11);
			if (!double.TryParse(InLine.Substring(48, 6), out maxMag))
			{
				maxMag = -20.0;
			}
			else
			{
				maxMagPlaces = Utilities.DecimalPlaces(InLine.Substring(48, 6));
			}
			string text = InLine.Substring(54, 7);
			minMagFlag = " ";
			if (text.Contains("("))
			{
				minMagFlag = "(";
			}
			if (text.Contains("<"))
			{
				minMagFlag = "<";
			}
			text = text.Replace("(", " ").Replace("<", " ");
			if (!double.TryParse(text, out minMag))
			{
				minMag = -20.0;
			}
			else
			{
				minMagPlaces = Utilities.DecimalPlaces(text);
			}
			photoSystem = InLine.Substring(61, 2);
			if (!double.TryParse(InLine.Substring(63, 14), out epoch))
			{
				epoch = 0.0;
			}
			else
			{
				epochPlaces = Utilities.DecimalPlaces(InLine.Substring(63, 14));
			}
			if (!double.TryParse(InLine.Substring(77, 14), out period))
			{
				period = 0.0;
			}
			else
			{
				periodPlaces = Utilities.DecimalPlaces(InLine.Substring(77, 14));
			}
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("{0,6:F0} ", xz);
			stringBuilder.Append(name.PadRight(30));
			stringBuilder.Append(VType.PadRight(11).Substring(0, 11));
			if (maxMag < -10.0)
			{
				stringBuilder.Append("      ");
			}
			else
			{
				stringBuilder.Append(Utilities.FormatNumber(maxMag, 2, maxMagPlaces).PadRight(6));
			}
			stringBuilder.Append(minMagFlag);
			if (minMag < -10.0)
			{
				stringBuilder.Append("      ");
			}
			else
			{
				stringBuilder.Append(Utilities.FormatNumber(minMag, 2, minMagPlaces).PadRight(6));
			}
			stringBuilder.Append(photoSystem);
			if (epoch <= 0.0)
			{
				stringBuilder.Append("".PadRight(14));
			}
			else
			{
				stringBuilder.Append(Utilities.FormatNumber(epoch, 7, epochPlaces).PadRight(14));
			}
			if (period <= 0.0)
			{
				stringBuilder.Append("".PadRight(14));
			}
			else
			{
				stringBuilder.Append(Utilities.FormatNumber(period, 4, periodPlaces).PadRight(14));
			}
			stringBuilder.Append(" ");
			return stringBuilder.ToString();
		}

		public int CompareTo(object other)
		{
			return XZ.CompareTo(((XZVariables)other).XZ);
		}
	}
}
