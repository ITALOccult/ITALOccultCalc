using System;
using Occult.Properties;

namespace Occult.Star_Catalogues
{
	internal class IFindex : IComparable
	{
		internal static int SortField;

		private int row;

		private int numOfLines;

		private double rA;

		private double dec;

		private string name1 = "";

		private string name2 = "";

		private string hD = "";

		private string hIPTyc = "";

		public double RA
		{
			get
			{
				return rA;
			}
			set
			{
				rA = value;
			}
		}

		public double Dec
		{
			get
			{
				return dec;
			}
			set
			{
				dec = value;
			}
		}

		public string Name1
		{
			get
			{
				return name1;
			}
			set
			{
				name1 = value;
			}
		}

		public string Name2
		{
			get
			{
				return name2;
			}
			set
			{
				name2 = value;
			}
		}

		public string HD
		{
			get
			{
				return hD;
			}
			set
			{
				hD = value;
			}
		}

		public string HIPTyc
		{
			get
			{
				return hIPTyc;
			}
			set
			{
				hIPTyc = value;
			}
		}

		public int Row
		{
			get
			{
				return row;
			}
			set
			{
				row = value;
			}
		}

		public int NumOfLines
		{
			get
			{
				return numOfLines;
			}
			set
			{
				numOfLines = value;
			}
		}

		public int CompareTo(object other)
		{
			return SortField switch
			{
				0 => RA.CompareTo(((IFindex)other).RA), 
				1 => Name1.CompareTo(((IFindex)other).Name1), 
				2 => Name2.CompareTo(((IFindex)other).Name2), 
				3 => HD.CompareTo(((IFindex)other).HD), 
				4 => HIPTyc.CompareTo(((IFindex)other).HIPTyc), 
				_ => RA.CompareTo(((IFindex)other).RA), 
			};
		}

		public void ReadLine(string InLine)
		{
			try
			{
				rA = double.Parse(InLine.Substring(0, 2).Replace(" ", "0")) + double.Parse(InLine.Substring(2, 2).Replace(" ", "0")) / 60.0 + double.Parse(InLine.Substring(4, 5).Replace(" ", "0")) / 3600.0;
				dec = double.Parse(InLine.Substring(10, 2).Replace(" ", "0")) + double.Parse(InLine.Substring(12, 2).Replace(" ", "0")) / 60.0 + double.Parse(InLine.Substring(14, 4).Replace(" ", "0")) / 3600.0;
			}
			catch
			{
				_ = Settings.Default.Administrator;
			}
			if (InLine.Substring(9, 1) == "-")
			{
				dec = 0.0 - dec;
			}
			name1 = InLine.Substring(20, 26);
			name2 = InLine.Substring(46, 26);
			hD = InLine.Substring(72, 13);
			hIPTyc = InLine.Substring(85, 19);
		}
	}
}
