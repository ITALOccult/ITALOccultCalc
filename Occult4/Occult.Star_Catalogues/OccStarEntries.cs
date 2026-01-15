using System;

namespace Occult.Star_Catalogues
{
	internal class OccStarEntries : IComparable
	{
		private string OccLine;

		private int OccNumber;

		private int Arg1;

		public string OCCline
		{
			get
			{
				return OccLine;
			}
			set
			{
				OccLine = value;
			}
		}

		public int OCCNumber
		{
			get
			{
				return OccNumber;
			}
			set
			{
				OccNumber = value;
			}
		}

		public void XZ_OCCentry(string OCC)
		{
			OccLine = OCC;
			if (!int.TryParse(OCC.Substring(35, 4), out Arg1))
			{
				Arg1 = 10000;
			}
			OccNumber = Arg1;
		}

		public int CompareTo(object other)
		{
			return OCCNumber.CompareTo(((OccStarEntries)other).OCCNumber);
		}
	}
}
