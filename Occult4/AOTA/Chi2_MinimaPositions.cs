using System;

namespace AOTA
{
	internal class Chi2_MinimaPositions : IComparable
	{
		private int pos = -1;

		private double[] chi2_value = new double[63]
		{
			-1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0,
			-1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0,
			-1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0,
			-1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0,
			-1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0,
			-1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0,
			-1.0, -1.0, -1.0
		};

		internal static bool SortByPosition;

		internal static int Index;

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

		internal double Chi2_value
		{
			get
			{
				return chi2_value[Index];
			}
			set
			{
				chi2_value[Index] = value;
			}
		}

		public int CompareTo(object other)
		{
			if (SortByPosition)
			{
				return pos.CompareTo(((Chi2_MinimaPositions)other).Pos);
			}
			return Chi2_value.CompareTo(((Chi2_MinimaPositions)other).Chi2_value);
		}
	}
}
