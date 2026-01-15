using System;

namespace AOTA
{
	internal class Sdevs : IComparable
	{
		private int frame;

		private double sdevT;

		private double sdev1;

		private double sdev2;

		private double sdev3;

		internal static int SortField;

		internal static bool Comp1Used;

		internal static bool Comp2Used;

		internal static bool Comp3Used;

		internal int Frame
		{
			get
			{
				return frame;
			}
			set
			{
				frame = value;
			}
		}

		internal double SDevT
		{
			get
			{
				return sdevT;
			}
			set
			{
				sdevT = value;
			}
		}

		internal double SDev1
		{
			get
			{
				return sdev1;
			}
			set
			{
				sdev1 = value;
			}
		}

		internal double SDev2
		{
			get
			{
				return sdev2;
			}
			set
			{
				sdev2 = value;
			}
		}

		internal double SDev3
		{
			get
			{
				return sdev3;
			}
			set
			{
				sdev3 = value;
			}
		}

		internal string CompareAll
		{
			get
			{
				string text = string.Format("{0,5}{1,7:f1}", frame, 0.0 - sdevT);
				text = ((!Comp1Used) ? (text + "        ") : (text + string.Format("{0,8:f1}", 0.0 - sdev1)));
				text = ((!Comp2Used) ? (text + "        ") : (text + string.Format("{0,8:f1}", 0.0 - sdev2)));
				if (Comp3Used)
				{
					return text + string.Format("{0,8:f1}", 0.0 - sdev3);
				}
				return text + "        ";
			}
		}

		internal string CompareComp1 => string.Format("{0,5}{1,7:f1}", frame, 0.0 - sdev1);

		internal string CompareComp2 => string.Format("{0,5}{1,7:f1}", frame, 0.0 - sdev2);

		internal string CompareComp3 => string.Format("{0,5}{1,7:f1}", frame, 0.0 - sdev3);

		public int CompareTo(object other)
		{
			if (SortField == 0)
			{
				return SDevT.CompareTo(((Sdevs)other).SDevT);
			}
			if (SortField == 1)
			{
				return SDev1.CompareTo(((Sdevs)other).SDev1);
			}
			if (SortField == 2)
			{
				return SDev2.CompareTo(((Sdevs)other).SDev2);
			}
			if (SortField == 3)
			{
				return SDev3.CompareTo(((Sdevs)other).SDev3);
			}
			return Frame.CompareTo(((Sdevs)other).Frame);
		}
	}
}
