using System;

namespace Occult
{
	internal class U4Tyc2 : IComparable
	{
		private int tycho2;

		private byte t2Component = 1;

		private int u4;

		public int Tycho2
		{
			get
			{
				return tycho2;
			}
			set
			{
				tycho2 = value;
			}
		}

		public byte T2Component
		{
			get
			{
				return t2Component;
			}
			set
			{
				t2Component = value;
			}
		}

		public int U4
		{
			get
			{
				return u4;
			}
			set
			{
				u4 = value;
			}
		}

		public int CompareTo(object other)
		{
			if (Tycho2 == ((U4Tyc2)other).Tycho2)
			{
				return T2Component.CompareTo(((U4Tyc2)other).T2Component);
			}
			return Tycho2.CompareTo(((U4Tyc2)other).Tycho2);
		}
	}
}
