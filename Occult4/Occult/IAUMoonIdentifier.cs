namespace Occult
{
	internal class IAUMoonIdentifier
	{
		private int astNumber;

		private string moonName = "";

		private string moonID = "";

		private string designation = "";

		internal int AstNumber
		{
			get
			{
				return astNumber;
			}
			set
			{
				astNumber = value;
			}
		}

		internal string MoonName
		{
			get
			{
				return moonName;
			}
			set
			{
				moonName = value;
			}
		}

		internal string MoonID
		{
			get
			{
				return moonID;
			}
			set
			{
				moonID = value;
			}
		}

		internal string Designation
		{
			get
			{
				return designation;
			}
			set
			{
				designation = value;
			}
		}

		internal string MoonIdentification
		{
			get
			{
				if (MoonName.Contains("S/"))
				{
					return "(" + AstNumber + ") " + MoonName;
				}
				return "(" + AstNumber + ") " + MoonID + " = " + MoonName;
			}
		}

		public override string ToString()
		{
			return MoonIdentification;
		}
	}
}
