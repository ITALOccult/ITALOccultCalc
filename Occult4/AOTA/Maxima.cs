using System;

namespace AOTA
{
	internal class Maxima : IComparable
	{
		private int pos = -1;

		private int width = -1;

		private double fom = -1.0;

		internal static bool SortToLower;

		internal int Pos
		{
			get
			{
				return pos;
			}
			set
			{
				pos = value;
			}
		}

		internal int Width
		{
			get
			{
				return width;
			}
			set
			{
				width = value;
			}
		}

		internal double FOM
		{
			get
			{
				return fom;
			}
			set
			{
				fom = value;
			}
		}

		public int CompareTo(object other)
		{
			if (SortToLower)
			{
				return fom.CompareTo(((Maxima)other).FOM);
			}
			return ((Maxima)other).FOM.CompareTo(fom);
		}
	}
}
