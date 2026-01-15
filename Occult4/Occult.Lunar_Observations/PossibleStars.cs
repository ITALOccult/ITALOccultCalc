using System;
using System.Text;

namespace Occult.Lunar_Observations
{
	internal class PossibleStars : IComparable
	{
		private string number;

		private double mag;

		private double pA;

		private double residual;

		private int xZ;

		internal static int SortFlag;

		public string Number
		{
			get
			{
				return number;
			}
			set
			{
				number = value;
			}
		}

		public double Mag
		{
			get
			{
				return mag;
			}
			set
			{
				mag = value;
			}
		}

		public double PA
		{
			get
			{
				return pA;
			}
			set
			{
				pA = value;
			}
		}

		public double Residual
		{
			get
			{
				return residual;
			}
			set
			{
				residual = value;
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

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(Number.PadRight(8));
			stringBuilder.AppendFormat(" {0,4:F1} ", mag);
			stringBuilder.AppendFormat(" {0,5:F1} ", pA);
			stringBuilder.AppendFormat(" {0,5:F1} ", residual);
			if (Math.Abs(residual) < 5.0)
			{
				stringBuilder.Append("*");
			}
			return stringBuilder.ToString();
		}

		public int CompareTo(object other)
		{
			if (SortFlag == 0)
			{
				return Math.Abs(Residual).CompareTo(Math.Abs(((PossibleStars)other).Residual));
			}
			return Math.Abs(Mag).CompareTo(((PossibleStars)other).Mag);
		}
	}
}
