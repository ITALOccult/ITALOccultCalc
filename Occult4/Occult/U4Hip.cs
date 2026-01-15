using System;

namespace Occult
{
	internal class U4Hip : IComparable
	{
		private int hip;

		private int zone;

		private int rec;

		private int mpos;

		private double u4Mag;

		internal static bool SortByHip;

		internal static bool IncludemPos;

		public int Hip
		{
			get
			{
				return hip;
			}
			set
			{
				hip = value;
			}
		}

		public int Zone
		{
			get
			{
				return zone;
			}
			set
			{
				zone = value;
			}
		}

		public int Rec
		{
			get
			{
				return rec;
			}
			set
			{
				rec = value;
			}
		}

		public int Mpos
		{
			get
			{
				return mpos;
			}
			set
			{
				mpos = value;
			}
		}

		public double U4Mag
		{
			get
			{
				return u4Mag;
			}
			set
			{
				u4Mag = value;
			}
		}

		public int CompareTo(object other)
		{
			if (SortByHip)
			{
				if (Hip == ((U4Hip)other).Hip)
				{
					return U4Mag.CompareTo(((U4Hip)other).U4Mag);
				}
				return Hip.CompareTo(((U4Hip)other).Hip);
			}
			return Mpos.CompareTo(((U4Hip)other).Mpos);
		}

		public override string ToString()
		{
			if (IncludemPos)
			{
				return Mpos.ToString().PadLeft(6) + Zone.ToString().PadLeft(4) + Rec.ToString().PadLeft(7) + string.Format(" {0,4:F1}", U4Mag);
			}
			return Hip.ToString().PadLeft(7) + Zone.ToString().PadLeft(4) + Rec.ToString().PadLeft(7) + string.Format(" {0,4:F1}", U4Mag);
		}
	}
}
