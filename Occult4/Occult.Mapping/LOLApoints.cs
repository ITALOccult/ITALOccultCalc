using System;

namespace Occult.Mapping
{
	internal class LOLApoints : IComparable
	{
		private double p;

		private double d;

		private double ht;

		public double Ht
		{
			get
			{
				return ht;
			}
			set
			{
				ht = value;
			}
		}

		public double P
		{
			get
			{
				return p;
			}
			set
			{
				p = value;
			}
		}

		public double D
		{
			get
			{
				return d;
			}
			set
			{
				d = value;
			}
		}

		public int CompareTo(object other)
		{
			if (D.CompareTo(((LOLApoints)other).D) == 0)
			{
				return P.CompareTo(((LOLApoints)other).P);
			}
			return D.CompareTo(((LOLApoints)other).D);
		}
	}
}
