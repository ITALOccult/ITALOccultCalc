using System;

namespace Occult
{
	public class TInvertTycho : IComparable
	{
		private int t2Num;

		private int seqNum;

		public int T2Num
		{
			get
			{
				return t2Num;
			}
			set
			{
				t2Num = value;
			}
		}

		public int SeqNum
		{
			get
			{
				return seqNum;
			}
			set
			{
				seqNum = value;
			}
		}

		public int CompareTo(object other)
		{
			return t2Num.CompareTo(((TInvertTycho)other).t2Num);
		}
	}
}
